{-# Language OverloadedStrings #-}
module Main where

import qualified Tokenizer
import qualified Parser

import           Text.Printf
import           System.Environment
import           System.IO
import           Control.Monad
import           Control.Monad.State.Strict

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO

import qualified Data.Map                      as M

import qualified Text.Megaparsec               as MP

ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr

ePutTextLn :: T.Text -> IO ()
ePutTextLn = TIO.hPutStrLn stderr

tshow :: Show a => a -> T.Text
tshow = T.pack . show

codePrelude :: [String]
codePrelude = [".intel_syntax noprefix", ".global main", "main:"]

popStack, pushStack, cmpThenPush :: [String]
popStack = ["  pop rdi", "  pop rax"]
pushStack = ["  push rax"]
cmpThenPush = ["  sete al", "  movzb rax, al"] ++ pushStack

data Scope = Scope { variables :: M.Map String Int, nextOffset :: Int }
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

genCode :: Parser.Ast -> CodeGen [String]
genCode (Parser.Num n) = return [printf "  push %d" n]
genCode (Parser.Assign (Parser.Ident ident) rval) = do
  doesVarExist <- M.member ident . variables <$> get
  unless doesVarExist $ createNewVar ident
  addrCode <- getAddress ident
  rvalCode <- genCode rval
  return
    $  addrCode
    ++ rvalCode
    ++ popStack
    ++ ["  mov [rax], rdi", "  push rdi"]
genCode (Parser.Assign l _) =
  genCodeError
    $ "expected left-hand in assignment expression to be an identifier but found: "
    ++ show l
genCode (Parser.Return a) = do
  aCode <- genCode a
  return $ aCode ++ ["  pop rax", "  mov rsp, rbp", "  pop rbp", "  ret"]
genCode (Parser.Ident ident) = do
  addrCode <- getAddress ident
  return $ addrCode ++ ["  pop rax", "  mov rax, [rax]"] ++ pushStack
genCode (Parser.Neg a) = do
  aCode <- genCode a
  return $ aCode ++ ["  pop rax", "  imul rax, -1"] ++ pushStack
genCode (Parser.Add a b) = do
  abCode <- genTwoPop a b
  return $ abCode ++ ["  add rax, rdi"] ++ pushStack
genCode (Parser.Sub a b) = do
  abCode <- genTwoPop a b
  return $ abCode ++ ["  sub rax, rdi"] ++ pushStack
genCode (Parser.Mul a b) = do
  abCode <- genTwoPop a b
  return $ abCode ++ ["  imul rax, rdi"] ++ pushStack
genCode (Parser.Div a b) = do
  abCode <- genTwoPop a b
  return $ abCode ++ ["  cqo", "  idiv rdi"] ++ pushStack
genCode (Parser.L a b) = do
  abCode <- genTwoPop a b
  return $ abCode ++ ["  setl rax, rdi"] ++ cmpThenPush
genCode (Parser.LEq a b) = do
  abCode <- genTwoPop a b
  return $ abCode ++ ["  setle rax, rdi"] ++ cmpThenPush
genCode (Parser.G   a b) = genCode (Parser.L b a)
genCode (Parser.GEq a b) = genCode (Parser.LEq b a)
genCode (Parser.Eq  a b) = do
  abCode <- genTwoPop a b
  return $ abCode ++ ["  cmp rax, rdi"] ++ cmpThenPush
genCode (Parser.Neq a b) = do
  abCode <- genTwoPop a b
  return $ abCode ++ ["  setne rax, rdi"] ++ cmpThenPush

genTwoPop :: Parser.Ast -> Parser.Ast -> CodeGen [String]
genTwoPop a b = do
  aCode <- genCode a
  bCode <- genCode b
  return $ aCode ++ bCode ++ popStack

generateCode :: [Parser.Ast] -> Either String [String]
generateCode program = do
  (code, scope) <- runStateT (traverse genCode program) initScope
  return
    $  codePrelude
    ++ [ "  push rbp"
       , "  mov rbp, rsp"
       , "  sub rsp, " ++ show (nextOffset scope)
       , "  # end of prelude"
       ]
    ++ concatMap (\c -> c ++ ["  pop rax"]) code
    ++ ["  # start of postlude", "  mov rsp, rbp", "  pop rbp", "  ret"]

runCompiler :: IO ()
runCompiler = do
  args <- getArgs
  case args of
    [arg] -> case Tokenizer.run "args" $ T.pack arg of
      Left  err    -> ePutStrLn $ MP.errorBundlePretty err
      Right tokens -> case Parser.run "tokens" tokens of
        Left  err     -> ePutStrLn $ MP.errorBundlePretty err
        Right program -> case generateCode program of
          Left err -> ePutStrLn err
          Right code ->
            --print ast
            forM_ code putStrLn
    _ -> ePutStrLn "invalid number of arguments"

main :: IO ()
main = runCompiler
