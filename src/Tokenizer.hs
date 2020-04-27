{-# Language OverloadedStrings #-}
module Tokenizer
  ( run
  , Token(..)
  , getLineOfToken
  , getSourcePosOfToken
  )
where

import           Data.Functor                   ( ($>) )

import qualified Data.Char                     as C
import qualified Data.Void                     as V
import qualified Data.Text                     as T

import           Text.Megaparsec                ( (<?>)
                                                , (<|>)
                                                )
import qualified Text.Megaparsec               as MP
import qualified Text.Megaparsec.Char          as MP

type Parser = MP.Parsec V.Void T.Text

data LineState = LineState
  { line :: T.Text
  , sourcePos :: !MP.SourcePos
  }
  deriving (Eq, Ord)

data Token
  = Number Int !LineState
  | Ident String !LineState
  | Return !LineState
  | LParen !LineState
  | RParen !LineState
  | Plus !LineState
  | Minus !LineState
  | Slash !LineState
  | Asterisk !LineState
  | L !LineState
  | LEq !LineState
  | G !LineState
  | GEq !LineState
  | Eq !LineState
  | Neq !LineState
  | Assign !LineState
  | Semicolon !LineState
  deriving (Eq, Ord)

instance Show Token where
  show (Number n  _) = show n
  show (Ident  ch _) = show ch
  show (Return    _) = "return"
  show (LParen    _) = "("
  show (RParen    _) = ")"
  show (Plus      _) = "+"
  show (Minus     _) = "-"
  show (Slash     _) = "/"
  show (Asterisk  _) = "*"
  show (L         _) = "<"
  show (LEq       _) = "<="
  show (G         _) = ">"
  show (GEq       _) = ">="
  show (Eq        _) = "=="
  show (Neq       _) = "!="
  show (Assign    _) = "="
  show (Semicolon _) = ";"

getLineState :: Token -> LineState
getLineState (Number _ ls ) = ls
getLineState (Ident  _ ls ) = ls
getLineState (Return    ls) = ls
getLineState (LParen    ls) = ls
getLineState (RParen    ls) = ls
getLineState (Plus      ls) = ls
getLineState (Minus     ls) = ls
getLineState (Slash     ls) = ls
getLineState (Asterisk  ls) = ls
getLineState (L         ls) = ls
getLineState (LEq       ls) = ls
getLineState (G         ls) = ls
getLineState (GEq       ls) = ls
getLineState (Eq        ls) = ls
getLineState (Neq       ls) = ls
getLineState (Assign    ls) = ls
getLineState (Semicolon ls) = ls

getLineOfToken :: Token -> String
getLineOfToken = T.unpack . line . getLineState

getSourcePosOfToken :: Token -> MP.SourcePos
getSourcePosOfToken = sourcePos . getLineState

getLineStatep :: Parser LineState
getLineStatep = do
  ps <- MP.statePosState <$> MP.getParserState
  return $ LineState (MP.pstateInput ps) (MP.pstateSourcePos ps)

whitespace :: Parser ()
whitespace = ($> ()) . MP.many . MP.oneOf $ [' ', '\t', '\n']

reserved :: Parser Token
reserved =
  foldr1 (<|>)
    . fmap (\(t, s) -> (t <$ MP.string s) <*> getLineStatep)
    $ [ (LParen   , "(")
      , (RParen   , ")")
      , (Plus     , "+")
      , (Minus    , "-")
      , (Slash    , "/")
      , (Asterisk , "*")
      , (LEq      , "<=")
      , (L        , "<")
      , (GEq      , ">=")
      , (G        , ">")
      , (Eq       , "==")
      , (Neq      , "!=")
      , (Assign   , "=")
      , (Semicolon, ";")
      , (Return   , "return")
      ]

ident :: Parser Token
ident =
  Ident
    <$> MP.some (MP.satisfy C.isAsciiLower)
    <*> getLineStatep
    <?> "variable name"

nonDigitChar :: Parser Char
nonDigitChar =
  MP.satisfy (\ch -> ch /= '0' && C.isDigit ch) <?> "non-zero digit"

number :: Parser Token
number = Number . read <$> numberString <*> getLineStatep
 where
  numberString =
    ((:) <$> nonDigitChar <*> MP.many MP.digitChar) <|> (pure <$> MP.digitChar)

token :: Parser Token
token = reserved <|> number <|> ident

tokens :: Parser [Token]
tokens = token `MP.sepEndBy` whitespace <* MP.eof

run :: String -> T.Text -> Either (MP.ParseErrorBundle T.Text V.Void) [Token]
run = MP.parse tokens
