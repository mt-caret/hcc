{-# Language LambdaCase #-}
{-# Language FlexibleInstances #-}
{-# Language TypeFamilies #-}
module Parser where

import           Prelude                 hiding ( div )

import qualified Tokenizer

import           Data.Proxy
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.List

import qualified Data.Void                     as V
import qualified Data.Set                      as S
import qualified Data.List.NonEmpty            as NE
--import qualified Data.Text                     as T

import           Text.Megaparsec                ( (<|>) )

import qualified Text.Megaparsec               as MP
import qualified Text.Megaparsec.Error         as MPE

type Parser = MP.Parsec V.Void CTokens

data Ast
  = Add Ast Ast
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

newtype CTokens = CTokens [ Tokenizer.Token ]

instance MP.Stream CTokens where
  type Token CTokens = Tokenizer.Token
  type Tokens CTokens = [Tokenizer.Token]
  tokenToChunk Proxy = return
  tokensToChunk Proxy = id
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  take1_ (CTokens []      ) = Nothing
  take1_ (CTokens (t : ts)) = Just (t, CTokens ts)
  takeN_ n (CTokens []) | n > 0 = Nothing
  takeN_ n (CTokens xs)         = Just . fmap CTokens $ splitAt n xs
  takeWhile_ p (CTokens ts) = CTokens <$> go ts []
   where
    go [] accum = (reverse accum, [])
    go (x : xs) accum =
      if p x then go xs (x : accum) else (reverse accum, x : xs)
  showTokens Proxy (t :| []) = show t
  showTokens Proxy (t :| ts) = intercalate ", " $ show <$> (t : ts)
  reachOffset o ps =
    ( offendingLine
    , MP.PosState { MP.pstateInput      = CTokens rawTokens
                  , MP.pstateOffset     = totalOffset
                  , MP.pstateSourcePos  = newSourcePos
                  , MP.pstateTabWidth   = MP.pstateTabWidth ps
                  , MP.pstateLinePrefix = MP.pstateLinePrefix ps
                  }
    )
   where
    (CTokens rawTokens) = MP.pstateInput ps
    totalOffset         = o + MP.pstateOffset ps
    currentToken        = if totalOffset >= length rawTokens
      then Nothing
      else Just $ rawTokens !! totalOffset
    offendingLine =
      maybe "<empty string>" Tokenizer.getLineOfToken currentToken
    newSourcePos =
      maybe (MP.pstateSourcePos ps) Tokenizer.getSourcePosOfToken currentToken


number :: Parser Int
number = MP.token test expected
 where
  test (Tokenizer.Number n _) = Just n
  test _                      = Nothing
  expected = S.singleton . MPE.Label . NE.fromList $ "number"

lparen :: Parser ()
lparen = () <$ MP.satisfy
  (\case
    Tokenizer.LParen _ -> True
    _                  -> False
  )

rparen :: Parser ()
rparen = () <$ MP.satisfy
  (\case
    Tokenizer.RParen _ -> True
    _                  -> False
  )

plus :: Parser ()
plus = () <$ MP.satisfy
  (\case
    Tokenizer.Plus _ -> True
    _                -> False
  )

minus :: Parser ()
minus = () <$ MP.satisfy
  (\case
    Tokenizer.Minus _ -> True
    _                 -> False
  )


slash :: Parser ()
slash = () <$ MP.satisfy
  (\case
    Tokenizer.Slash _ -> True
    _                 -> False
  )

asterisk :: Parser ()
asterisk = () <$ MP.satisfy
  (\case
    Tokenizer.Asterisk _ -> True
    _                    -> False
  )

l :: Parser ()
l = () <$ MP.satisfy
  (\case
    Tokenizer.L _ -> True
    _             -> False
  )

leq :: Parser ()
leq = () <$ MP.satisfy
  (\case
    Tokenizer.LEq _ -> True
    _               -> False
  )


g :: Parser ()
g = () <$ MP.satisfy
  (\case
    Tokenizer.G _ -> True
    _             -> False
  )

geq :: Parser ()
geq = () <$ MP.satisfy
  (\case
    Tokenizer.GEq _ -> True
    _               -> False
  )

eq :: Parser ()
eq = () <$ MP.satisfy
  (\case
    Tokenizer.Eq _ -> True
    _              -> False
  )

neq :: Parser ()
neq = () <$ MP.satisfy
  (\case
    Tokenizer.Neq _ -> True
    _               -> False
  )

-- left-associative binary op which parses the following: base (`op` base)*
binOpMany :: Parser a -> Parser (a -> a) -> Parser a
binOpMany base op = foldl (\x f -> f x) <$> base <*> MP.many op

-- takes a left-associative binary op (like Add) and
-- creates a parser that takes a parser for the binary operator,
-- parses the next primitive, and partially applies it to the
-- second argument
applyR :: (a -> a -> a) -> Parser b -> Parser a -> Parser (a -> a)
applyR op opParser base = flip op <$> (opParser *> base)

ast :: Parser Ast
ast = expr <* MP.eof

expr :: Parser Ast
expr = equality

equality :: Parser Ast
equality = binOpMany base op
 where
  base = relational
  eq_  = applyR Eq eq base
  neq_ = applyR Neq neq base
  op   = eq_ <|> neq_

relational :: Parser Ast
relational = binOpMany base op
 where
  base = add
  l_   = applyR L l base
  leq_ = applyR LEq leq base
  g_   = applyR G g base
  geq_ = applyR GEq geq base
  op   = l_ <|> leq_ <|> g_ <|> geq_

add :: Parser Ast
add = binOpMany base op
 where
  base = mul
  add_ = applyR Add plus base
  sub  = applyR Sub minus base
  op   = add_ <|> sub

mul :: Parser Ast
mul = binOpMany base op
 where
  base = unary
  mul_ = applyR Mul asterisk base
  div  = applyR Div slash base
  op   = mul_ <|> div

unary :: Parser Ast
unary = op <*> primary where op = (id <$ plus) <|> (Neg <$ minus) <|> pure id

primary :: Parser Ast
primary = (Num <$> number) <|> MP.between lparen rparen expr

run
  :: String
  -> [Tokenizer.Token]
  -> Either (MP.ParseErrorBundle CTokens V.Void) Ast
run name tokens = MP.parse ast name (CTokens tokens)
