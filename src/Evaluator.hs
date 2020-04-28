{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Evaluator where

import qualified Ast
import Control.Monad.State.Strict
import qualified Data.Map as M

data Scope = Scope {variables :: M.Map String Int, returnedValue :: Maybe Int}

type AstEval a = StateT Scope (Either String) a

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

liftLeft :: String -> AstEval a
liftLeft = StateT . const . Left

run :: [AstEval Int] -> Either String (Maybe Int)
run ast = returnedValue <$> execStateT (sequenceA ast) (Scope M.empty Nothing)

instance Ast.AstSym (AstEval Int) where
  identS name = do
    vars <- variables <$> get
    case M.lookup name vars of
      Just val -> return val
      Nothing -> liftLeft $ "variable " ++ show name ++ " not found"
  assignS name a = do
    aVal <- a
    modify (\(Scope vars ret) -> Scope (M.insert name aVal vars) ret)
    return aVal
  returnS a = do
    aVal <- a
    modify
      ( \case
          Scope vars Nothing -> Scope vars (Just aVal)
          x -> x
      )
    return aVal
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

