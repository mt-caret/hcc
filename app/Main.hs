{-# Language OverloadedStrings #-}
module Main where

import qualified Tokenizer
import qualified Parser

import           Text.Printf
import           System.Environment
import           System.IO
import           Control.Monad

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO

import qualified Text.Megaparsec               as MP

ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr

ePutTextLn :: T.Text -> IO ()
ePutTextLn = TIO.hPutStrLn stderr

tshow :: Show a => a -> T.Text
tshow = T.pack . show

emitCode :: [Tokenizer.Token] -> Either T.Text [String]
emitCode []                          = Left "No tokens found"
emitCode (Tokenizer.Number n _ : xs) = do
  ops <- go xs
  return $ (:) (printf "  mov rax, %d" n) ops
 where
  go [] = Right []
  go (Tokenizer.Plus _ : Tokenizer.Number n2 _ : ys) =
    (:) (printf "  add rax, %d" n2) <$> go ys
  go (Tokenizer.Minus _ : Tokenizer.Number n2 _ : ys) =
    (:) (printf "  sub rax, %d" n2) <$> go ys
  go ys = Left $ "unexpected: " <> tshow ys
emitCode (t : _) = Left ("unexpected operator: " <> tshow t)

runTokenizer :: IO ()
runTokenizer = do
  args <- getArgs
  case args of
    [arg] -> case Tokenizer.run "args" $ T.pack arg of
      Left  err    -> ePutStrLn $ MP.errorBundlePretty err
      Right tokens -> case emitCode tokens of
        Left  err  -> ePutTextLn err
        Right code -> do
          forM_ (codePrelude ++ code) putStrLn
          putStrLn "  ret"
    _ -> ePutStrLn "invalid number of arguments"

codePrelude :: [String]
codePrelude = [".intel_syntax noprefix", ".global main", "main:"]

popStack, pushStack, cmpThenPush :: [String]
popStack = ["  pop rdi", "  pop rax"]
pushStack = ["  push rax"]
cmpThenPush = ["  sete al", "  movzb rax, al"] ++ pushStack

genCode :: Parser.Ast -> [String]
genCode (Parser.Num n) = [printf "  push %d" n]
genCode (Parser.Neg a) =
  genCode a ++ ["  pop rax", "  imul rax, -1"] ++ pushStack
genCode (Parser.Add a b) = genTwoPop a b ++ ["  add rax, rdi"] ++ pushStack
genCode (Parser.Sub a b) = genTwoPop a b ++ ["  sub rax, rdi"] ++ pushStack
genCode (Parser.Mul a b) = genTwoPop a b ++ ["  imul rax, rdi"] ++ pushStack
genCode (Parser.Div a b) =
  genTwoPop a b ++ ["  cqo", "  idiv rdi"] ++ pushStack
genCode (Parser.L   a b) = genTwoPop a b ++ ["  setl rax, rdi"] ++ cmpThenPush
genCode (Parser.LEq a b) = genTwoPop a b ++ ["  setle rax, rdi"] ++ cmpThenPush
genCode (Parser.G   a b) = genCode (Parser.L b a)
genCode (Parser.GEq a b) = genCode (Parser.LEq b a)
genCode (Parser.Eq  a b) = genTwoPop a b ++ ["  cmp rax, rdi"] ++ cmpThenPush
genCode (Parser.Neq a b) = genTwoPop a b ++ ["  setne rax, rdi"] ++ cmpThenPush

genTwoPop :: Parser.Ast -> Parser.Ast -> [String]
genTwoPop a b = genCode a ++ genCode b ++ popStack

main :: IO ()
main = do
  args <- getArgs
  case args of
    [arg] -> case Tokenizer.run "args" $ T.pack arg of
      Left  err    -> ePutStrLn $ MP.errorBundlePretty err
      Right tokens -> case Parser.run "tokens" tokens of
        Left  err -> ePutStrLn $ MP.errorBundlePretty err
        Right ast -> do
          --print ast
          forM_ (codePrelude ++ genCode ast) putStrLn
          putStrLn "  pop rax"
          putStrLn "  ret"
    _ -> ePutStrLn "invalid number of arguments"
