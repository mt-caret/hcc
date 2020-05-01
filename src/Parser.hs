{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Parser where

import qualified Ast
import Control.Applicative
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import qualified Data.Set as S
import qualified Data.Void as V
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Error as MPE
import qualified Tokenizer
import Prelude hiding (div)

type Parser = MP.Parsec V.Void CTokens

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

number :: Ast.AstSym ast => Parser ast
number = Ast.numS <$> MP.token test expected
  where
    test (Tokenizer.Token (Tokenizer.Number n) _) = Just n
    test _ = Nothing
    expected = S.singleton . MPE.Label . NE.fromList $ "number"

return_,
  if_,
  else_,
  while_,
  for_,
  comma,
  lparen,
  rparen,
  lbrack,
  rbrack,
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
if_ = () <$ MP.satisfy ((==) Tokenizer.If . Tokenizer.tokenType)
else_ = () <$ MP.satisfy ((==) Tokenizer.Else . Tokenizer.tokenType)
while_ = () <$ MP.satisfy ((==) Tokenizer.While . Tokenizer.tokenType)
for_ = () <$ MP.satisfy ((==) Tokenizer.For . Tokenizer.tokenType)
comma = () <$ MP.satisfy ((==) Tokenizer.Comma . Tokenizer.tokenType)
lparen = () <$ MP.satisfy ((==) Tokenizer.LParen . Tokenizer.tokenType)
rparen = () <$ MP.satisfy ((==) Tokenizer.RParen . Tokenizer.tokenType)
lbrack = () <$ MP.satisfy ((==) Tokenizer.LBrack . Tokenizer.tokenType)
rbrack = () <$ MP.satisfy ((==) Tokenizer.RBrack . Tokenizer.tokenType)
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

program :: Ast.AstSym ast => Parser [ast]
program = MP.some stmt <* MP.eof

parens :: Parser a -> Parser a
parens = MP.between lparen rparen

stmt :: Ast.AstSym ast => Parser ast
stmt = singleStmt <|> blockStmt <|> ifStmt <|> whileStmt <|> forStmt <|> returnStmt
  where
    singleStmt = expr <* semicolon
    blockStmt = Ast.blockS <$> MP.between lbrack rbrack (MP.many stmt)
    returnStmt = Ast.returnS <$> MP.between return_ semicolon expr
    ifStmt = do
      ifExpr <- if_ *> parens expr
      ifBody <- stmt
      (Ast.ifelseS ifExpr ifBody <$> (else_ *> stmt)) <|> pure (Ast.ifS ifExpr ifBody)
    whileStmt = Ast.whileS <$> (while_ *> parens expr) <*> stmt
    forStmt = do
      for_
      lparen
      a <- MP.optional expr
      semicolon
      b <- MP.optional expr
      semicolon
      c <- MP.optional expr
      rparen
      Ast.forS a b c <$> stmt

expr :: Ast.AstSym ast => Parser ast
expr = assign

assign :: Ast.AstSym ast => Parser ast
assign = MP.try (Ast.assignS <$> ident <*> (assign_ *> assign)) <|> equality

equality :: Ast.AstSym ast => Parser ast
equality = binOpMany base op
  where
    base = relational
    eq_ = applyR Ast.eqS eq base
    neq_ = applyR Ast.neqS neq base
    op = eq_ <|> neq_

relational :: Ast.AstSym ast => Parser ast
relational = binOpMany base op
  where
    base = add
    l_ = applyR Ast.lS l base
    leq_ = applyR Ast.leqS leq base
    g_ = applyR Ast.gS g base
    geq_ = applyR Ast.geqS geq base
    op = l_ <|> leq_ <|> g_ <|> geq_

add :: Ast.AstSym ast => Parser ast
add = binOpMany base op
  where
    base = mul
    add_ = applyR Ast.addS plus base
    sub = applyR Ast.subS minus base
    op = add_ <|> sub

mul :: Ast.AstSym ast => Parser ast
mul = binOpMany base op
  where
    base = unary
    mul_ = applyR Ast.mulS asterisk base
    div = applyR Ast.divS slash base
    op = mul_ <|> div

unary :: Ast.AstSym ast => Parser ast
unary = op <*> primary where op = (id <$ plus) <|> (Ast.negS <$ minus) <|> pure id

call :: Ast.AstSym ast => Parser ast
call = Ast.callS <$> ident <*> parens (MP.sepBy expr comma)

primary :: Ast.AstSym ast => Parser ast
primary = number <|> MP.try call <|> Ast.identS <$> ident <|> parens expr

run ::
  Ast.AstSym ast =>
  String ->
  [Tokenizer.Token] ->
  Either (MP.ParseErrorBundle CTokens V.Void) [ast]
run name tokens = MP.parse program name (CTokens tokens)
