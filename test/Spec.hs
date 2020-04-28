{-# LANGUAGE OverloadedStrings #-}

import qualified CodeGen
import Control.Monad.Except
import Control.Monad.Managed
import Data.Bifunctor
import qualified Data.Text as T
import qualified Parser
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.Megaparsec as MP
import qualified Tokenizer
import Turtle
import Prelude hiding (FilePath)

runAssembler :: MonadIO io => Shell Line -> ExceptT String io FilePath
runAssembler asm = ExceptT $ do
  filePath <- liftIO $ with (mktempfile "/tmp" "hcc") return
  let filePath_ = T.pack . encodeString $ filePath
  (exitCode, _, se) <- procStrictWithErr "gcc" ["-x", "assembler", "-o", filePath_, "-"] asm
  return $
    case exitCode of
      ExitSuccess -> Right filePath
      ExitFailure _ -> Left $ T.unpack se

runBinary :: MonadIO io => FilePath -> io ExitCode
runBinary filePath = proc (T.pack . encodeString $ filePath) [] (select [])

exitCodeToInt :: ExitCode -> Int
exitCodeToInt ExitSuccess = 0
exitCodeToInt (ExitFailure n) = n

parse :: Parser.AstSym ast => T.Text -> Either String [ast]
parse cCode = do
  tokens <- first MP.errorBundlePretty $ Tokenizer.run "args" cCode
  first MP.errorBundlePretty $ Parser.run "tokens" tokens

toShellLine :: [String] -> Maybe (Shell Line)
toShellLine = fmap select . traverse (textToLine . T.pack)

compareOutput :: Text -> IO (Either String (Maybe Int, Maybe Int))
compareOutput cCode = runExceptT $ do
  evalResult <- liftEither $ Parser.evaluate =<< parseResult
  asm <- liftEither $ CodeGen.generateCode =<< parseResult
  binPath <-
    runAssembler
      =<< liftEither
        ( case toShellLine asm of
            Nothing -> Left "toShellLine failed"
            Just xs -> Right xs
        )
  runResult <- return . exitCodeToInt <$> runBinary binPath
  return (evalResult, runResult)
  where
    parseResult :: Parser.AstSym ast => Either String [ast]
    parseResult = parse cCode

runCompare :: String -> IO ()
runCompare cCode = do
  compareResult <- compareOutput $ T.pack cCode
  case compareResult of
    Left errMsg -> assertFailure errMsg
    Right (evalResult, runResult) -> evalResult @?= runResult

compareTest :: TestName -> TestTree
compareTest cCode = testCase cCode $ runCompare cCode

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ compareTest "return 1;",
      compareTest "return 7 - 8 + 3;",
      compareTest "a = 1; b = 2; return (a == b) + a;",
      compareTest "returnx = 4; return returnx;"
    ]

main :: IO ()
main = defaultMain unitTests
