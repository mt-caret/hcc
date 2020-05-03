{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import qualified Ast
import qualified CodeGen
import Control.Lens hiding ((<.>))
import Control.Monad.Except
import Control.Monad.Managed
import Data.Bifunctor
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Evaluator
import qualified NeatInterpolation
import qualified Parser
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.Megaparsec as MP
import qualified Tokenizer
import Turtle
import Prelude hiding (FilePath)

filePathToString :: FilePath -> T.Text
filePathToString = T.pack . encodeString

functions :: Shell Line
functions =
  select . NE.toList . textToLines $
    [NeatInterpolation.text|
    #include <stdio.h>
    void args0() {
      printf("OK\n");
    }
    void args1(int a) {
      printf("OK, %d\n", a);
    }
    void args2(int a, int b) {
      printf("OK, %d, %d\n", a, b);
    }
    void args3(int a, int b, int c) {
      printf("OK, %d, %d, %d\n", a, b, c);
    }
    void args4(int a, int b, int c, int d) {
      printf("OK, %d, %d, %d, %d\n", a, b, c, d);
    }
    void args5(int a, int b, int c, int d, int e) {
      printf("OK, %d, %d, %d, %d, %d\n", a, b, c, d, e);
    }
    void args6(int a, int b, int c, int d, int e, int f) {
      printf("OK, %d, %d, %d, %d, %d, %d\n", a, b, c, d, e, f);
    }
  |]

errorOr :: String -> a -> (ExitCode, Text, Text) -> Either String a
errorOr _ a (ExitSuccess, _, _) = Right a
errorOr location _ (ExitFailure _, _, se) = Left $ location ++ ": " ++ T.unpack se

compileFunctions :: MonadIO io => FilePath -> ExceptT String io ()
compileFunctions filePath =
  ExceptT $
    errorOr "compileFunctions" ()
      <$> procStrictWithErr
        "gcc"
        ["-c", "-x", "c", "-o", filePathToString filePath, "-"]
        functions

compileAssembly ::
  MonadIO io =>
  FilePath ->
  Shell Line ->
  ExceptT String io ()
compileAssembly filePath asm =
  ExceptT $
    errorOr "compileAssembly" ()
      <$> procStrictWithErr
        "gcc"
        ["-c", "-x", "assembler", "-o", filePathToString filePath, "-"]
        asm

linkObjects :: MonadIO io => FilePath -> [FilePath] -> ExceptT String io ()
linkObjects binPath objectPaths =
  ExceptT $
    errorOr "linkObjects" ()
      <$> procStrictWithErr
        "gcc"
        ((filePathToString <$> objectPaths) ++ ["-o", filePathToString binPath])
        (select [])

genTmp :: MonadIO io => io FilePath
genTmp = liftIO $ with (mktempfile "/tmp" "hcc") return

intoRight :: Monad m => m a -> ExceptT String m a
intoRight = ExceptT . fmap Right

compileBinary :: MonadIO io => Shell Line -> ExceptT String io FilePath
compileBinary asm = do
  asmObjectPath <- (<.> "o") <$> intoRight genTmp
  compileAssembly asmObjectPath asm
  functionObjPath <- (<.> "o") <$> intoRight genTmp
  compileFunctions functionObjPath
  binPath <- intoRight genTmp
  linkObjects binPath [asmObjectPath, functionObjPath]
  return binPath

exitCodeToInt :: ExitCode -> Int
exitCodeToInt ExitSuccess = 0
exitCodeToInt (ExitFailure n) = n

runBinary :: MonadIO io => FilePath -> io (Int, Text)
runBinary binPath = do
  (exitCode, so, _) <- procStrictWithErr (filePathToString binPath) [] (select [])
  return (exitCodeToInt exitCode, so)

parse :: Ast.AstSym ast => T.Text -> Either String [ast]
parse cCode = do
  tokens <- first MP.errorBundlePretty $ Tokenizer.run "args" cCode
  first MP.errorBundlePretty $ Parser.run "tokens" cCode tokens

toShellLine :: [String] -> Either String (Shell Line)
toShellLine str =
  case fmap select . traverse (textToLine . T.pack) $ str of
    Nothing -> Left "toShellLine failed"
    Just xs -> Right xs

compareOutput :: Text -> IO (Either String ((Maybe Int, Text), (Maybe Int, Text)))
compareOutput cCode = runExceptT $ do
  evalResult <- liftEither $ Evaluator.run =<< parseResult
  asm <- liftEither $ CodeGen.generateCode =<< parseResult
  binPath <- compileBinary <=< liftEither . toShellLine $ asm
  runResult <- runBinary binPath
  return (evalResult, over _1 Just runResult)
  where
    parseResult :: Ast.AstSym ast => Either String [ast]
    parseResult = parse cCode

runCompareEq :: String -> (Int, Text) -> IO ()
runCompareEq cCode val = do
  compareResult <- compareOutput $ T.pack cCode
  case compareResult of
    Left errMsg -> assertFailure errMsg
    Right (evalResult, runResult) -> do
      evalResult @?= over _1 Just val
      runResult @?= evalResult

compareTest :: TestName -> (Int, Text) -> TestTree
compareTest cCode val = testCase cCode $ runCompareEq cCode val

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    $ fmap
      (uncurry compareTest)
      [ ("return 1;", (1, "")),
        ("return 0 < 1;", (1, "")),
        ("return 0 <= 0;", (1, "")),
        ("return 7 - 8 + 3;", (2, "")),
        ("return 7 - 8 + -3 + 5;", (1, "")),
        ("return 7 / (3 - 8) * 2 + 3;", (1, "")),
        ("a = 1; b = 2; return (a == b) + a;", (1, "")),
        ("Weird_var = 1; return Weird_var;", (1, "")),
        ("returnx = 4; return returnx;", (4, "")),
        ("ifx = 4; return ifx;", (4, "")),
        ("if (1 > 0) return 2;", (2, "")),
        ("if (0 >= 1) return 2; return 1;", (1, "")),
        ("if (1 != 0) return 2; else return 1;", (2, "")),
        ("if (0) return 2; else return 1;", (1, "")),
        ("a = 10; while (a) a = a - 1; return a;", (0, "")),
        ("a = 0; for (i = 0; i < 10; i = i + 1) a = a + 2; return a;", (20, "")),
        ("a = 0; i = 0; while (i < 10) { a = a + 2; i = i + 1; } return a;", (20, "")),
        ("a = b = 1; b = a + b; return b;", (2, "")),
        ("args0(); return 1;", (1, "OK\n")),
        ("a = 1; args1(a); return 1;", (1, "OK, 1\n")),
        ("a = 1; b = 2; args2(a, b); return 1;", (1, "OK, 1, 2\n")),
        ("a = 1; b = 2; c = 3; args3(a, b, c); return 1;", (1, "OK, 1, 2, 3\n")),
        ("a = 1; b = 2; c = 3; d = 4; args4(a, b, c, d); return 1;", (1, "OK, 1, 2, 3, 4\n")),
        ("a = 1; b = 2; c = 3; d = 4; e = 5; args5(a, b, c, d, e); return 1;", (1, "OK, 1, 2, 3, 4, 5\n")),
        ("a = 1; b = 2; c = 3; d = 4; e = 5; f = 6; args6(a, b, c, d, e, f); return 1;", (1, "OK, 1, 2, 3, 4, 5, 6\n"))
      ]

main :: IO ()
main = defaultMain unitTests
