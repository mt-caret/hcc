{-# LANGUAGE TemplateHaskell #-}

module CodeGen where

import qualified Ast
import Control.Lens
import Control.Monad.State.Strict
import qualified Data.Map as M
import Text.Printf

codePrelude :: [String]
codePrelude = [".intel_syntax noprefix", ".global main", "main:"]

popStack, pushStack, cmpThenPush, lThenPush, leThenPush :: [String]
popStack = ["  pop rdi", "  pop rax"]
pushStack = ["  push rax"]
cmpThenPush = ["  sete al", "  movzb rax, al"] ++ pushStack
lThenPush = ["  setl al", "  movzb rax, al"] ++ pushStack
leThenPush = ["  setle al", "  movzb rax, al"] ++ pushStack

data Scope
  = Scope
      { _variables :: M.Map String Int,
        _nextOffset :: Int,
        _counter :: Int
      }

$(makeLenses ''Scope)

type CodeGen a = StateT Scope (Either String) a

initScope :: Scope
initScope = Scope M.empty 0 0

createNewVar :: String -> CodeGen ()
createNewVar name =
  modify $ \(Scope v no c) -> Scope (M.insert name no v) (no + 8) c

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

genCode :: Ast.Ast -> CodeGen [String]
genCode (Ast.Num n) = return [printf "  push %d" n]
genCode (Ast.Assign ident rval) = do
  doesVarExist <- M.member ident . view variables <$> get
  unless doesVarExist $ createNewVar ident
  addrCode <- getAddress ident
  rvalCode <- genCode rval
  return $
    addrCode
      ++ rvalCode
      ++ popStack
      ++ ["  mov [rax], rdi", "  push rdi"]
genCode (Ast.Block xs) = concat <$> traverse genCode xs
genCode (Ast.Return a) = do
  aCode <- genCode a
  return $ aCode ++ ["  pop rax", "  mov rsp, rbp", "  pop rbp", "  ret"]
genCode (Ast.Call name args)
  | length args > 6 =
    genCodeError $
      printf "invalid function: %s (only functions with up to six arguments are supported)" name
genCode (Ast.Call name args) =
  genCall <$> traverse genCode args
  where
    popToReg reg code = code ++ ["  pop " ++ reg]
    genCall = (++ ["  call " ++ name]) . concat . zipWith popToReg ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]
genCode (Ast.If p a) = do
  pCode <- genCode p
  endLabel <- createLabel "end"
  aCode <- genCode a
  return $ pCode ++ ["  pop rax", "  cmp rax, 0", "je  " ++ endLabel] ++ aCode ++ [endLabel ++ ":"]
genCode (Ast.IfElse p a b) = do
  pCode <- genCode p
  elseLabel <- createLabel "else"
  endLabel <- createLabel "end"
  aCode <- genCode a
  bCode <- genCode b
  return $
    pCode
      ++ ["  pop rax", "  cmp rax, 0", "je  " ++ elseLabel]
      ++ aCode
      ++ ["  jmp " ++ endLabel, elseLabel ++ ":"]
      ++ bCode
      ++ [endLabel ++ ":"]
genCode (Ast.While p a) = do
  pCode <- genCode p
  beginLabel <- createLabel "begin"
  endLabel <- createLabel "end"
  aCode <- genCode a
  return $
    [beginLabel ++ ":"]
      ++ pCode
      ++ ["  pop rax", "  cmp rax, 0", "je  " ++ endLabel]
      ++ aCode
      ++ ["  jmp " ++ beginLabel, endLabel ++ ":"]
genCode (Ast.For start p end a) = do
  startCode <- maybe (return []) genCode start
  beginLabel <- createLabel "begin"
  pCode <- maybe (return ["  push 1"]) genCode p
  endCode <- maybe (return []) genCode end
  endLabel <- createLabel "end"
  aCode <- genCode a
  return $ surround "for" $
    surround "start" startCode
      ++ [beginLabel ++ ":"]
      ++ surround "pCode" pCode
      ++ ["  pop rax", "  cmp rax, 0", "je  " ++ endLabel]
      ++ surround "aCode" aCode
      ++ surround "endCode" endCode
      ++ ["  jmp " ++ beginLabel, endLabel ++ ":"]
genCode (Ast.Ident ident) = do
  addrCode <- getAddress ident
  return $ addrCode ++ ["  pop rax", "  mov rax, [rax]"] ++ pushStack
genCode (Ast.Neg a) = do
  aCode <- genCode a
  return $ aCode ++ ["  pop rax", "  imul rax, -1"] ++ pushStack
genCode (Ast.Add a b) = do
  abCode <- genTwoPop a b
  return $ abCode ++ ["  add rax, rdi"] ++ pushStack
genCode (Ast.Sub a b) = do
  abCode <- genTwoPop a b
  return $ abCode ++ ["  sub rax, rdi"] ++ pushStack
genCode (Ast.Mul a b) = do
  abCode <- genTwoPop a b
  return $ abCode ++ ["  imul rax, rdi"] ++ pushStack
genCode (Ast.Div a b) = do
  abCode <- genTwoPop a b
  return $ abCode ++ ["  cqo", "  idiv rdi"] ++ pushStack
genCode (Ast.L a b) = do
  abCode <- genTwoPop a b
  return $ abCode ++ ["  cmp rax, rdi"] ++ lThenPush
genCode (Ast.LEq a b) = do
  abCode <- genTwoPop a b
  return $ abCode ++ ["  cmp rax, rdi"] ++ leThenPush
genCode (Ast.G a b) = genCode (Ast.L b a)
genCode (Ast.GEq a b) = genCode (Ast.LEq b a)
genCode (Ast.Eq a b) = do
  abCode <- genTwoPop a b
  return $ abCode ++ ["  cmp rax, rdi"] ++ cmpThenPush
genCode (Ast.Neq a b) = do
  abCode <- genTwoPop a b
  return $ abCode ++ ["  setne rax, rdi"] ++ cmpThenPush

genTwoPop :: Ast.Ast -> Ast.Ast -> CodeGen [String]
genTwoPop a b = do
  aCode <- genCode a
  bCode <- genCode b
  return $ aCode ++ bCode ++ popStack

generateCode :: [Ast.Ast] -> Either String [String]
generateCode program = do
  (code, scope) <- runStateT (traverse genCode program) initScope
  return $
    codePrelude
      ++ [ "  push rbp",
           "  mov rbp, rsp",
           "  sub rsp, " ++ show (scope ^. nextOffset),
           "# end of prelude"
         ]
      ++ concatMap (\c -> c ++ ["  pop rax"]) code
      ++ ["# start of postlude", "  mov rsp, rbp", "  pop rbp", "  ret"]
