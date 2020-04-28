{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Parser where

import Control.Applicative
import Control.Monad.State.Strict
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Proxy
import qualified Data.Set as S
import qualified Data.Void as V
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Error as MPE
import qualified Tokenizer
import Prelude hiding (div)
import qualified Prelude

type Parser = MP.Parsec V.Void CTokens

data Ast
  = Ident String
  | Assign String Ast
  | Return Ast
  | Add Ast Ast
  | Sub Ast Ast
  | Mul Ast Ast
  | Div Ast Ast
  | L Ast Ast
  | LEq Ast Ast
  | G Ast Ast
  | GEq Ast Ast
  | Eq Ast Ast
  | Neq Ast Ast
  | Neg Ast
  | Num Int
  deriving (Show, Eq)

class AstSym a where
  identS :: String -> a
  assignS :: String -> a -> a
  returnS :: a -> a
  addS :: a -> a -> a
  subS :: a -> a -> a
  mulS :: a -> a -> a
  divS :: a -> a -> a
  lS :: a -> a -> a
  leqS :: a -> a -> a
  gS :: a -> a -> a
  geqS :: a -> a -> a
  eqS :: a -> a -> a
  neqS :: a -> a -> a
  negS :: a -> a
  numS :: Int -> a

instance AstSym Ast where
  identS = Ident
  assignS = Assign
  returnS = Return
  addS = Add
  subS = Sub
  mulS = Mul
  divS = Div
  lS = L
  leqS = LEq
  gS = G
  geqS = GEq
  eqS = Eq
  neqS = Neq
  negS = Neg
  numS = Num

data Scope = Scope {variables :: M.Map String Int, returnedValue :: Maybe Int}

type AstEval a = StateT Scope (Either String) a

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

liftLeft :: String -> AstEval a
liftLeft = StateT . const . Left

evaluate :: [AstEval Int] -> Either String (Maybe Int)
evaluate ast = returnedValue <$> execStateT (sequenceA ast) (Scope M.empty Nothing)

instance AstSym (AstEval Int) where
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
  addS a b = (+) <$> a <*> b
  subS a b = (-) <$> a <*> b
  mulS a b = (*) <$> a <*> b
  divS a b = Prelude.div <$> a <*> b
  lS a b = fmap boolToInt $ (<) <$> a <*> b
  leqS a b = fmap boolToInt $ (<=) <$> a <*> b
  gS a b = fmap boolToInt $ (>) <$> a <*> b
  geqS a b = fmap boolToInt $ (>=) <$> a <*> b
  eqS a b = fmap boolToInt $ (==) <$> a <*> b
  neqS a b = fmap boolToInt $ (/=) <$> a <*> b
  negS = fmap negate
  numS = return

newtype CTokens = CTokens [Tokenizer.Token]

instance MP.Stream CTokens where
  type Token CTokens = Tokenizer.Token
  type Tokens CTokens = [Tokenizer.Token]
  tokenToChunk Proxy = return
  tokensToChunk Proxy = id
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  take1_ (CTokens []) = Nothing
  take1_ (CTokens (t : ts)) = Just (t, CTokens ts)
  takeN_ n (CTokens []) | n > 0 = Nothing
  takeN_ n (CTokens xs) = Just . fmap CTokens $ splitAt n xs
  takeWhile_ p (CTokens ts) = CTokens <$> go ts []
    where
      go [] accum = (reverse accum, [])
      go (x : xs) accum =
        if p x then go xs (x : accum) else (reverse accum, x : xs)
  showTokens Proxy (t :| []) = show t
  showTokens Proxy (t :| ts) = intercalate ", " $ show <$> (t : ts)
  reachOffset o ps =
    ( offendingLine,
      MP.PosState
        { MP.pstateInput = CTokens rawTokens,
          MP.pstateOffset = totalOffset,
          MP.pstateSourcePos = newSourcePos,
          MP.pstateTabWidth = MP.pstateTabWidth ps,
          MP.pstateLinePrefix = MP.pstateLinePrefix ps
        }
    )
    where
      (CTokens rawTokens) = MP.pstateInput ps
      totalOffset = o + MP.pstateOffset ps
      currentToken =
        if totalOffset >= length rawTokens
          then Nothing
          else Just $ rawTokens !! totalOffset
      offendingLine =
        maybe "<empty string>" Tokenizer.getLineOfToken currentToken
      newSourcePos =
        maybe (MP.pstateSourcePos ps) Tokenizer.getSourcePosOfToken currentToken

ident :: Parser String
ident = MP.token test expected
  where
    test (Tokenizer.Token (Tokenizer.Ident str) _) = Just str
    test _ = Nothing
    expected = S.singleton . MPE.Label . NE.fromList $ "identifier"

number :: AstSym ast => Parser ast
number = numS <$> MP.token test expected
  where
    test (Tokenizer.Token (Tokenizer.Number n) _) = Just n
    test _ = Nothing
    expected = S.singleton . MPE.Label . NE.fromList $ "number"

return_,
  lparen,
  rparen,
  plus,
  minus,
  slash,
  asterisk,
  l,
  leq,
  g,
  geq,
  eq,
  neq,
  assign_,
  semicolon ::
    Parser ()
return_ = () <$ MP.satisfy ((==) Tokenizer.Return . Tokenizer.tokenType)
lparen = () <$ MP.satisfy ((==) Tokenizer.LParen . Tokenizer.tokenType)
rparen = () <$ MP.satisfy ((==) Tokenizer.RParen . Tokenizer.tokenType)
plus = () <$ MP.satisfy ((==) Tokenizer.Plus . Tokenizer.tokenType)
minus = () <$ MP.satisfy ((==) Tokenizer.Minus . Tokenizer.tokenType)
slash = () <$ MP.satisfy ((==) Tokenizer.Slash . Tokenizer.tokenType)
asterisk = () <$ MP.satisfy ((==) Tokenizer.Asterisk . Tokenizer.tokenType)
l = () <$ MP.satisfy ((==) Tokenizer.L . Tokenizer.tokenType)
leq = () <$ MP.satisfy ((==) Tokenizer.LEq . Tokenizer.tokenType)
g = () <$ MP.satisfy ((==) Tokenizer.G . Tokenizer.tokenType)
geq = () <$ MP.satisfy ((==) Tokenizer.GEq . Tokenizer.tokenType)
eq = () <$ MP.satisfy ((==) Tokenizer.Eq . Tokenizer.tokenType)
neq = () <$ MP.satisfy ((==) Tokenizer.Neq . Tokenizer.tokenType)
assign_ = () <$ MP.satisfy ((==) Tokenizer.Assign . Tokenizer.tokenType)
semicolon = () <$ MP.satisfy ((==) Tokenizer.Semicolon . Tokenizer.tokenType)

-- left-associative binary op which parses the following: base (`op` base)*
binOpMany :: Parser a -> Parser (a -> a) -> Parser a
binOpMany base op = foldl (\x f -> f x) <$> base <*> MP.many op

-- takes a left-associative binary op (like Add) and
-- creates a parser that takes a parser for the binary operator,
-- parses the next primitive, and partially applies it to the
-- second argument
applyR :: (a -> a -> a) -> Parser b -> Parser a -> Parser (a -> a)
applyR op opParser base = flip op <$> (opParser *> base)

program :: AstSym ast => Parser [ast]
program = MP.some stmt <* MP.eof

stmt :: AstSym ast => Parser ast
stmt = expr <* semicolon <|> (returnS <$> MP.between return_ semicolon expr)

expr :: AstSym ast => Parser ast
expr = assign

assign :: AstSym ast => Parser ast
assign = MP.try (assignS <$> ident <*> (assign_ *> assign)) <|> equality

equality :: AstSym ast => Parser ast
equality = binOpMany base op
  where
    base = relational
    eq_ = applyR eqS eq base
    neq_ = applyR neqS neq base
    op = eq_ <|> neq_

relational :: AstSym ast => Parser ast
relational = binOpMany base op
  where
    base = add
    l_ = applyR lS l base
    leq_ = applyR leqS leq base
    g_ = applyR gS g base
    geq_ = applyR geqS geq base
    op = l_ <|> leq_ <|> g_ <|> geq_

add :: AstSym ast => Parser ast
add = binOpMany base op
  where
    base = mul
    add_ = applyR addS plus base
    sub = applyR subS minus base
    op = add_ <|> sub

mul :: AstSym ast => Parser ast
mul = binOpMany base op
  where
    base = unary
    mul_ = applyR mulS asterisk base
    div = applyR divS slash base
    op = mul_ <|> div

unary :: AstSym ast => Parser ast
unary = op <*> primary where op = (id <$ plus) <|> (negS <$ minus) <|> pure id

primary :: AstSym ast => Parser ast
primary = number <|> identS <$> ident <|> MP.between lparen rparen expr

run ::
  AstSym ast =>
  String ->
  [Tokenizer.Token] ->
  Either (MP.ParseErrorBundle CTokens V.Void) [ast]
run name tokens = MP.parse program name (CTokens tokens)
