module CodeGen where

import qualified Ast
import Control.Monad.State.Strict
import qualified Data.Map as M
import Text.Printf

codePrelude :: [String]
codePrelude = [".intel_syntax noprefix", ".global main", "main:"]

popStack, pushStack, cmpThenPush :: [String]
popStack = ["  pop rdi", "  pop rax"]
pushStack = ["  push rax"]
cmpThenPush = ["  sete al", "  movzb rax, al"] ++ pushStack

data Scope = Scope {variables :: M.Map String Int, nextOffset :: Int}

type CodeGen a = StateT Scope (Either String) a

initScope :: Scope
initScope = Scope M.empty 0

createNewVar :: String -> CodeGen ()
createNewVar name =
  modify (\(Scope v no) -> Scope (M.insert name no v) (no + 8))

genCodeError :: String -> CodeGen a
genCodeError = StateT . const . Left

getAddress :: String -> CodeGen [String]
getAddress name = do
  vars <- variables <$> get
  case M.lookup name vars of
    Nothing -> genCodeError $ "variable " ++ name ++ " not found"
    Just offset ->
      return ["  mov rax, rbp", "  sub rax, " ++ show offset, "  push rax"]

genCode :: Ast.Ast -> CodeGen [String]
genCode (Ast.Num n) = return [printf "  push %d" n]
genCode (Ast.Assign ident rval) = do
  doesVarExist <- M.member ident . variables <$> get
  unless doesVarExist $ createNewVar ident
  addrCode <- getAddress ident
  rvalCode <- genCode rval
  return $
    addrCode
      ++ rvalCode
      ++ popStack
      ++ ["  mov [rax], rdi", "  push rdi"]
genCode (Ast.Return a) = do
  aCode <- genCode a
  return $ aCode ++ ["  pop rax", "  mov rsp, rbp", "  pop rbp", "  ret"]
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
  return $ abCode ++ ["  setl rax, rdi"] ++ cmpThenPush
genCode (Ast.LEq a b) = do
  abCode <- genTwoPop a b
  return $ abCode ++ ["  setle rax, rdi"] ++ cmpThenPush
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
           "  sub rsp, " ++ show (nextOffset scope),
           "  # end of prelude"
         ]
      ++ concatMap (\c -> c ++ ["  pop rax"]) code
      ++ ["  # start of postlude", "  mov rsp, rbp", "  pop rbp", "  ret"]
