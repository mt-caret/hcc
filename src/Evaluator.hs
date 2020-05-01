{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Evaluator where

import qualified Ast
import Control.Lens
import Control.Monad.State.Strict
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Printf

data Scope
  = Scope
      { _functions :: M.Map String ([Int] -> StateT Scope (Either String) Int),
        _variables :: M.Map String Int,
        _returnedValue :: Maybe Int,
        _stdout :: [String]
      }

$(makeLenses ''Scope)

type AstEval a = StateT Scope (Either String) a

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

liftLeft :: String -> AstEval a
liftLeft = StateT . const . Left

printToStdout :: String -> AstEval ()
printToStdout str =
  modify . over stdout $ (:) str

functions_ :: M.Map String ([Int] -> AstEval Int)
functions_ =
  M.fromList
    [ ( "args0",
        \case
          [] -> 0 <$ printToStdout (printf "OK")
          xs -> liftLeft $ "expected 0 arguments but found: " ++ show (length xs)
      ),
      ( "args1",
        \case
          [x] -> 0 <$ printToStdout (printf "OK, %d" x)
          xs -> liftLeft $ "expected 1 argument but found: " ++ show (length xs)
      ),
      ( "args2",
        \case
          [x, y] -> 0 <$ printToStdout (printf "OK, %d, %d" x y)
          xs -> liftLeft $ "expected 1 argument but found: " ++ show (length xs)
      )
    ]

run :: [AstEval Int] -> Either String (Maybe Int, T.Text)
run ast = do
  scope <- execStateT (sequenceA ast) (Scope functions_ M.empty Nothing [])
  return (scope ^. returnedValue, T.pack . unlines . reverse $ scope ^. stdout)

instance Ast.AstSym (AstEval Int) where
  identS name = do
    vars <- view variables <$> get
    case M.lookup name vars of
      Just val -> return val
      Nothing -> liftLeft $ "variable " ++ show name ++ " not found"
  blockS asts = 0 <$ sequence_ asts
  assignS name a = do
    aVal <- a
    modify . over variables $ M.insert name aVal
    return aVal
  returnS a = do
    aVal <- a
    modify . over returnedValue $
      \case
        Nothing -> Just aVal
        x -> x
    return aVal
  callS name args = do
    funcs <- view functions <$> get
    argVals <- sequence args
    case M.lookup name funcs of
      Just f -> f argVals
      Nothing -> liftLeft $ "function " ++ name ++ " not found"
  ifS p a = do
    pVal <- p
    if pVal /= 0 then a else return 0
  ifelseS p a b = do
    pVal <- p
    if pVal /= 0 then a else b
  whileS p a = do
    pVal <- p
    if pVal /= 0
      then a *> Ast.whileS p a
      else return 0
  forS start p end a = do
    sequence_ start
    go
    where
      go :: AstEval Int
      go = do
        pVal <- maybe (return True) (fmap (/= 0)) p
        if pVal
          then do
            _ <- a
            sequence_ end
            go
          else return 0
  addS a b = (+) <$> a <*> b
  subS a b = (-) <$> a <*> b
  mulS a b = (*) <$> a <*> b
  divS a b = div <$> a <*> b
  lS a b = fmap boolToInt $ (<) <$> a <*> b
  leqS a b = fmap boolToInt $ (<=) <$> a <*> b
  gS a b = fmap boolToInt $ (>) <$> a <*> b
  geqS a b = fmap boolToInt $ (>=) <$> a <*> b
  eqS a b = fmap boolToInt $ (==) <$> a <*> b
  neqS a b = fmap boolToInt $ (/=) <$> a <*> b
  negS = fmap negate
  numS = return

