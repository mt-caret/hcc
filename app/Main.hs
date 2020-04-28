{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified CodeGen
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Parser
import System.Environment
import System.IO
import qualified Text.Megaparsec as MP
import qualified Tokenizer

ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr

ePutTextLn :: T.Text -> IO ()
ePutTextLn = TIO.hPutStrLn stderr

tshow :: Show a => a -> T.Text
tshow = T.pack . show

runCompiler :: IO ()
runCompiler = do
  args <- getArgs
  case args of
    [arg] -> case Tokenizer.run "args" $ T.pack arg of
      Left err -> ePutStrLn $ MP.errorBundlePretty err
      Right tokens -> case Parser.run "tokens" tokens of
        Left err -> ePutStrLn $ MP.errorBundlePretty err
        Right program -> case CodeGen.generateCode program of
          Left err -> ePutStrLn err
          Right code ->
            --print ast
            forM_ code putStrLn
    _ -> ePutStrLn "invalid number of arguments"

main :: IO ()
main = runCompiler
