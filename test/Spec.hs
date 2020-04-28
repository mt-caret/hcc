{-# LANGUAGE OverloadedStrings #-}

import qualified Ast
import qualified CodeGen
import Control.Monad.Except
import Control.Monad.Managed
import Data.Bifunctor
import qualified Data.Text as T
import qualified Evaluator
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

parse :: Ast.AstSym ast => T.Text -> Either String [ast]
parse cCode = do
  tokens <- first MP.errorBundlePretty $ Tokenizer.run "args" cCode
  first MP.errorBundlePretty $ Parser.run "tokens" tokens

toShellLine :: [String] -> Maybe (Shell Line)
toShellLine = fmap select . traverse (textToLine . T.pack)

compareOutput :: Text -> IO (Either String (Maybe Int, Maybe Int))
compareOutput cCode = runExceptT $ do
  evalResult <- liftEither $ Evaluator.run =<< parseResult
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
    parseResult :: Ast.AstSym ast => Either String [ast]
    parseResult = parse cCode

runCompareEq :: String -> Int -> IO ()
runCompareEq cCode val = do
  compareResult <- compareOutput $ T.pack cCode
  case compareResult of
    Left errMsg -> assertFailure errMsg
    Right (evalResult, runResult) -> do
      evalResult @?= Just val
      runResult @?= evalResult

compareTest :: TestName -> Int -> TestTree
compareTest cCode val = testCase cCode $ runCompareEq cCode val

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    $ fmap
      (uncurry compareTest)
      [ ("return 1;", 1),
        ("return 0 < 1;", 1),
        ("return 0 <= 0;", 1),
        ("return 7 - 8 + 3;", 2),
        ("a = 1; b = 2; return (a == b) + a;", 1),
        ("returnx = 4; return returnx;", 4),
        ("ifx = 4; return ifx;", 4),
        ("if (1) return 2;", 2),
        ("if (0) return 2; return 1;", 1),
        ("if (1) return 2; else return 1;", 2),
        ("if (0) return 2; else return 1;", 1),
        ("a = 10; while (a) a = a - 1; return a;", 0),
        ("a = 0; for (i = 0; i < 0; i = i + 1) a = a + 2; return a;", 0),
        ("a = 0; for (i = 0; i < 2; i = i + 1) a = a + 2; return a;", 4),
        ("a = 0; for (i = 0; i < 2; i = i + 1) a = a + 4; return a;", 8) -- this makes no sense!
      ]

main :: IO ()
main = defaultMain unitTests
