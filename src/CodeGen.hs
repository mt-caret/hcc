{-# LANGUAGE TemplateHaskell #-}

module CodeGen where

import qualified Ast
import Control.Applicative
import Control.Lens
import Control.Monad.State.Strict
import qualified Data.Map as M
import Text.Printf

data Scope
  = Scope
      { _variables :: M.Map String Int,
        _nextOffset :: Int,
        _counter :: Int,
        _aligned :: Bool
      }

$(makeLenses ''Scope)

type CodeGen a = StateT Scope (Either String) a

initScope :: Scope
initScope = Scope M.empty 0 0 True

data CanPushPop
  = Rax
  | Rdi
  | Rsi
  | Rdx
  | Rcx
  | R8
  | R9
  | Rsp
  | Rbp
  | Lit Int

instance Show CanPushPop where
  show Rax = "rax"
  show Rdi = "rdi"
  show Rsi = "rsi"
  show Rdx = "rdx"
  show Rcx = "rcx"
  show R8 = "r8"
  show R9 = "r9"
  show Rsp = "rsp"
  show Rbp = "rbp"
  show (Lit n) = printf "%d" n

push :: CanPushPop -> CodeGen [String]
push x = ["  push " ++ show x] <$ modify (over aligned not)

pop :: CanPushPop -> CodeGen [String]
pop x = ["  pop " ++ show x] <$ modify (over aligned not)

codePrelude :: [String]
codePrelude = [".intel_syntax noprefix", ".global main", "main:"]

popStack, pushStack, cmpThenPush, lThenPush, leThenPush, neqThenPush :: CodeGen [String]
popStack = (++) <$> pop Rdi <*> pop Rax
pushStack = push Rax
cmpThenPush = (["  sete al", "  movzb rax, al"] ++) <$> pushStack
lThenPush = (["  setl al", "  movzb rax, al"] ++) <$> pushStack
leThenPush = (["  setle al", "  movzb rax, al"] ++) <$> pushStack
neqThenPush = (["  setne al", "  movzb rax, al"] ++) <$> pushStack

createNewVar :: String -> CodeGen ()
createNewVar name =
  modify $ \(Scope v no c a) -> Scope (M.insert name no v) (no + 8) c a

createLabel :: String -> CodeGen String
createLabel labelName = do
  count <- printf "%05d" . view counter <$> get
  modify $ over counter (+ 1)
  return $ ".L__" ++ labelName ++ "__" ++ count

genCodeError :: String -> CodeGen a
genCodeError = StateT . const . Left

getAddress :: String -> CodeGen [String]
getAddress name = do
  vars <- view variables <$> get
  case M.lookup name vars of
    Nothing -> genCodeError $ "variable " ++ name ++ " not found"
    Just offset ->
      return ["  mov rax, rbp", "  sub rax, " ++ show offset, "  push rax"]

surround :: String -> [String] -> [String]
surround name code =
  ["# start of " ++ name] ++ code ++ ["# end of " ++ name]

crunch :: (Traversable t, Applicative f) => t (f [a]) -> f [a]
crunch = fmap concat . sequenceA

genCode :: Ast.Ast -> CodeGen [String]
genCode (Ast.Num n) = push (Lit n)
genCode (Ast.Assign ident rval) = do
  doesVarExist <- M.member ident . view variables <$> get
  unless doesVarExist $ createNewVar ident
  addrCode <- getAddress ident
  rvalCode <- genCode rval
  pCode <- popStack
  pushRdi <- push Rdi
  return $
    addrCode
      ++ rvalCode
      ++ pCode
      ++ ["  mov [rax], rdi"]
      ++ pushRdi
genCode (Ast.Block xs) = concat <$> traverse genCode xs
genCode (Ast.Return a) = do
  aCode <- genCode a
  popRax <- pop Rax
  popRbp <- pop Rbp
  return $ aCode ++ popRax ++ ["  mov rsp, rbp"] ++ popRbp ++ ["  ret"]
genCode (Ast.Call name args)
  | length args > 6 =
    genCodeError $
      printf "invalid function: %s (only functions with up to six arguments are supported)" name
genCode (Ast.Call name args) = do
  argCode <-
    sequenceA
      . zipWith (liftA2 (++)) (map genCode args)
      $ map pop [Rdi, Rsi, Rdx, Rcx, R8, R9]
  isAligned <- view aligned <$> get
  preCall <- if isAligned then return [] else push (Lit 1)
  postCall <- if isAligned then return [] else pop R9
  return $
    concat argCode ++ preCall ++ ["  call " ++ name] ++ postCall
genCode (Ast.If p a) = do
  pCode <- genCode p
  popRax <- pop Rax
  endLabel <- createLabel "end"
  aCode <- genCode a
  return $
    pCode
      ++ popRax
      ++ ["  cmp rax, 0", "je  " ++ endLabel]
      ++ aCode
      ++ [endLabel ++ ":"]
genCode (Ast.IfElse p a b) = do
  pCode <- genCode p
  popRax <- pop Rax
  elseLabel <- createLabel "else"
  endLabel <- createLabel "end"
  aCode <- genCode a
  bCode <- genCode b
  return $
    pCode
      ++ popRax
      ++ ["  cmp rax, 0", "je  " ++ elseLabel]
      ++ aCode
      ++ ["  jmp " ++ endLabel, elseLabel ++ ":"]
      ++ bCode
      ++ [endLabel ++ ":"]
genCode (Ast.While p a) = do
  pCode <- genCode p
  popRax <- pop Rax
  beginLabel <- createLabel "begin"
  endLabel <- createLabel "end"
  aCode <- genCode a
  return $
    [beginLabel ++ ":"]
      ++ pCode
      ++ popRax
      ++ ["  cmp rax, 0", "je  " ++ endLabel]
      ++ aCode
      ++ ["  jmp " ++ beginLabel, endLabel ++ ":"]
genCode (Ast.For start p end a) = do
  startCode <- maybe (return []) genCode start
  beginLabel <- createLabel "begin"
  pCode <- maybe (return ["  push 1"]) genCode p
  popRax <- pop Rax
  endCode <- maybe (return []) genCode end
  endLabel <- createLabel "end"
  aCode <- genCode a
  return $ surround "for" $
    surround "start" startCode
      ++ [beginLabel ++ ":"]
      ++ surround "pCode" pCode
      ++ popRax
      ++ ["  cmp rax, 0", "je  " ++ endLabel]
      ++ surround "aCode" aCode
      ++ surround "endCode" endCode
      ++ ["  jmp " ++ beginLabel, endLabel ++ ":"]
genCode (Ast.Ident ident) =
  crunch [getAddress ident, return ["  pop rax", "  mov rax, [rax]"], pushStack]
genCode (Ast.Neg a) =
  crunch [genCode a, return ["  pop rax", "  imul rax, -1"], pushStack]
genCode (Ast.Add a b) =
  crunch [genTwoPop a b, return ["  add rax, rdi"], pushStack]
genCode (Ast.Sub a b) =
  crunch [genTwoPop a b, return ["  sub rax, rdi"], pushStack]
genCode (Ast.Mul a b) =
  crunch [genTwoPop a b, return ["  imul rax, rdi"], pushStack]
genCode (Ast.Div a b) =
  crunch [genTwoPop a b, return ["  cqo", "  idiv rdi"], pushStack]
genCode (Ast.L a b) =
  crunch [genTwoPop a b, return ["  cmp rax, rdi"], lThenPush]
genCode (Ast.LEq a b) =
  crunch [genTwoPop a b, return ["  cmp rax, rdi"], leThenPush]
genCode (Ast.G a b) = genCode (Ast.L b a)
genCode (Ast.GEq a b) = genCode (Ast.LEq b a)
genCode (Ast.Eq a b) =
  crunch [genTwoPop a b, return ["  cmp rax, rdi"], cmpThenPush]
genCode (Ast.Neq a b) =
  crunch [genTwoPop a b, return ["  cmp rax, rdi"], neqThenPush]

genTwoPop :: Ast.Ast -> Ast.Ast -> CodeGen [String]
genTwoPop a b =
  crunch [genCode a, genCode b, popStack]

generateCode :: [Ast.Ast] -> Either String [String]
generateCode program = do
  let genCode' ast = (++) <$> genCode ast <*> pop Rax
  (code, scope) <- runStateT (traverse genCode' program) initScope
  return $
    codePrelude
      ++ [ "  push rbp",
           "  mov rbp, rsp",
           "  sub rsp, " ++ show (scope ^. nextOffset),
           "# end of prelude"
         ]
      ++ concat code
      ++ ["# start of postlude", "  mov rsp, rbp", "  pop rbp", "  ret"]
