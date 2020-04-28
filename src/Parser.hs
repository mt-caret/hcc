{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Parser where

import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import qualified Data.Set as S
import qualified Data.Void as V
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Error as MPE
import qualified Tokenizer
import Prelude hiding (div)

type Parser = MP.Parsec V.Void CTokens

data Ast
  = Ident String
  | Assign Ast Ast
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

ident :: Parser Ast
ident = Ident <$> MP.token test expected
  where
    test (Tokenizer.Token (Tokenizer.Ident str) _) = Just str
    test _ = Nothing
    expected = S.singleton . MPE.Label . NE.fromList $ "variable name"

number :: Parser Ast
number = Num <$> MP.token test expected
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

program :: Parser [Ast]
program = MP.some stmt <* MP.eof

stmt :: Parser Ast
stmt = expr <* semicolon <|> (Return <$> MP.between return_ semicolon expr)

expr :: Parser Ast
expr = assign

assign :: Parser Ast
assign = do
  lv <- equality
  (Assign lv <$> (assign_ *> assign)) <|> pure lv

equality :: Parser Ast
equality = binOpMany base op
  where
    base = relational
    eq_ = applyR Eq eq base
    neq_ = applyR Neq neq base
    op = eq_ <|> neq_

relational :: Parser Ast
relational = binOpMany base op
  where
    base = add
    l_ = applyR L l base
    leq_ = applyR LEq leq base
    g_ = applyR G g base
    geq_ = applyR GEq geq base
    op = l_ <|> leq_ <|> g_ <|> geq_

add :: Parser Ast
add = binOpMany base op
  where
    base = mul
    add_ = applyR Add plus base
    sub = applyR Sub minus base
    op = add_ <|> sub

mul :: Parser Ast
mul = binOpMany base op
  where
    base = unary
    mul_ = applyR Mul asterisk base
    div = applyR Div slash base
    op = mul_ <|> div

unary :: Parser Ast
unary = op <*> primary where op = (id <$ plus) <|> (Neg <$ minus) <|> pure id

primary :: Parser Ast
primary = number <|> ident <|> MP.between lparen rparen expr

run ::
  String ->
  [Tokenizer.Token] ->
  Either (MP.ParseErrorBundle CTokens V.Void) [Ast]
run name tokens = MP.parse program name (CTokens tokens)
