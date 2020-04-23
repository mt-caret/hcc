{-# Language OverloadedStrings #-}
module Main where

import           Lib
import qualified Tokenizer

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

emitCode :: [Tokenizer.Token] -> Either T.Text [String]
emitCode []                          = Left "No tokens found"
emitCode (Tokenizer.Reserved r : _ ) = Left ("unexpected operator: " <> r)
emitCode (Tokenizer.Number   n : xs) = do
  ops <- go xs
  return $ (:) (printf "  mov rax, %d" n) ops
 where
  go [] = Right []
  go (Tokenizer.Reserved "+" : Tokenizer.Number n : xs) =
    (:) (printf "  add rax, %d" n) <$> go xs
  go (Tokenizer.Reserved "-" : Tokenizer.Number n : xs) =
    (:) (printf "  sub rax, %d" n) <$> go xs
  go xs = Left $ "unexpected: " <> T.pack (show xs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [arg] -> case Tokenizer.run "args" $ T.pack arg of
      Left  err    -> ePutStrLn $ MP.errorBundlePretty err
      Right tokens -> case emitCode tokens of
        Left  err  -> ePutTextLn err
        Right code -> do
          putStrLn ".intel_syntax noprefix"
          putStrLn ".global main"
          putStrLn "main:"
          forM_ code putStrLn
          putStrLn "  ret"
    _ -> ePutStrLn "invalid number of arguments"
