{-# Language LambdaCase #-}
{-# Language FlexibleInstances #-}
{-# Language TypeFamilies #-}
module Parser where

import qualified Tokenizer

import           Data.Proxy
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.List

import qualified Data.Void                     as V
import qualified Data.Set                      as S
import qualified Data.List.NonEmpty            as NE
import qualified Data.Text                     as T

import           Text.Megaparsec                ( (<|>) )

import qualified Text.Megaparsec               as MP
import qualified Text.Megaparsec.Error         as MPE

type Parser = MP.Parsec V.Void [Tokenizer.Token]

data Ast
  = Add Ast Ast
  | Sub Ast Ast
  | Mul Ast Ast
  | Div Ast Ast
  | Num Int
  deriving (Show, Eq)

instance MP.Stream [Tokenizer.Token] where
  type Token [Tokenizer.Token] = Tokenizer.Token
  type Tokens [Tokenizer.Token] = [Tokenizer.Token]
  tokenToChunk Proxy = return
  tokensToChunk Proxy = id
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  take1_ []       = Nothing
  take1_ (t : ts) = Just (t, ts)
  takeN_ n [] | n > 0 = Nothing
  takeN_ n xs         = Just $ splitAt n xs
  takeWhile_ p ts = go p ts []
   where
    go _ [] accum = (reverse accum, [])
    go p (x : xs) accum =
      if p x then go p xs (x : accum) else (reverse accum, x : xs)
  showTokens Proxy (t :| []) = show t
  showTokens Proxy (t :| ts) = intercalate ", " $ show <$> (t : ts)
  reachOffset o ps =
    ( offendingLine
    , MP.PosState { MP.pstateInput      = rawTokens
                  , MP.pstateOffset     = totalOffset
                  , MP.pstateSourcePos  = newSourcePos
                  , MP.pstateTabWidth   = MP.pstateTabWidth ps
                  , MP.pstateLinePrefix = MP.pstateLinePrefix ps
                  }
    )
   where
    rawTokens    = MP.pstateInput ps
    totalOffset  = o + MP.pstateOffset ps
    currentToken = if totalOffset >= length rawTokens
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

primary :: Parser Ast
primary = (Num <$> number) <|> MP.between lparen rparen expr

mul :: Parser Ast
mul = do
  let mul = flip Mul <$> (asterisk *> primary)
  let div = flip Div <$> (slash *> primary)
  let op  = mul <|> div
  foldl (\x f -> f x) <$> primary <*> MP.many op

expr :: Parser Ast
expr = do
  let add = flip Add <$> (plus *> mul)
  let sub = flip Sub <$> (minus *> mul)
  let op  = add <|> sub
  foldl (\x f -> f x) <$> mul <*> MP.many op

ast :: Parser Ast
ast = expr <* MP.eof

run
  :: String
  -> [Tokenizer.Token]
  -> Either (MP.ParseErrorBundle [Tokenizer.Token] V.Void) Ast
run = MP.parse ast
