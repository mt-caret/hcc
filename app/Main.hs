{-# Language OverloadedStrings #-}
module Main where

import           Lib
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
  go (Tokenizer.Plus _ : Tokenizer.Number n _ : xs) =
    (:) (printf "  add rax, %d" n) <$> go xs
  go (Tokenizer.Minus _ : Tokenizer.Number n _ : xs) =
    (:) (printf "  sub rax, %d" n) <$> go xs
  go xs = Left $ "unexpected: " <> tshow xs
emitCode (t : _) = Left ("unexpected operator: " <> tshow t)

printPrelude :: IO ()
printPrelude = do
  putStrLn ".intel_syntax noprefix"
  putStrLn ".global main"
  putStrLn "main:"

runTokenizer :: IO ()
runTokenizer = do
  args <- getArgs
  case args of
    [arg] -> case Tokenizer.run "args" $ T.pack arg of
      Left  err    -> ePutStrLn $ MP.errorBundlePretty err
      Right tokens -> case emitCode tokens of
        Left  err  -> ePutTextLn err
        Right code -> do
          printPrelude
          forM_ code putStrLn
          putStrLn "  ret"
    _ -> ePutStrLn "invalid number of arguments"

popStack = ["  pop rdi", "  pop rax"]
pushStack = ["  push rax"]

genCode :: Parser.Ast -> [String]
genCode (Parser.Num n) = [printf "  push %d" n]
genCode (Parser.Add a b) =
  genCode a ++ genCode b ++ popStack ++ ["  add rax, rdi"] ++ pushStack
genCode (Parser.Sub a b) =
  genCode a ++ genCode b ++ popStack ++ ["  sub rax, rdi"] ++ pushStack
genCode (Parser.Mul a b) =
  genCode a ++ genCode b ++ popStack ++ ["  imul rax, rdi"] ++ pushStack
genCode (Parser.Div a b) =
  genCode a ++ genCode b ++ popStack ++ ["  cqo", "  idiv rdi"] ++ pushStack

main :: IO ()
main = do
  args <- getArgs
  case args of
    [arg] -> case Tokenizer.run "args" $ T.pack arg of
      Left  err    -> ePutStrLn $ MP.errorBundlePretty err
      Right tokens -> case Parser.run "tokens" tokens of
        Left  err -> ePutStrLn $ MP.errorBundlePretty err
        Right ast -> do
          print ast
          --printPrelude
          --forM_ (genCode ast) putStrLn
          --putStrLn "  pop rax"
          --putStrLn "  ret"
    _ -> ePutStrLn "invalid number of arguments"
