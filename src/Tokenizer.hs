{-# LANGUAGE OverloadedStrings #-}

module Tokenizer
  ( run,
    Token (..),
    TokenType (..),
    getLineOfToken,
    getSourcePosOfToken,
  )
where

import qualified Data.Char as C
import Data.Functor (($>))
import qualified Data.Text as T
import qualified Data.Void as V
import Text.Megaparsec ((<?>), (<|>))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP

type Parser = MP.Parsec V.Void T.Text

data LineState
  = LineState
      { line :: T.Text,
        sourcePos :: !MP.SourcePos
      }
  deriving (Eq, Ord)

data TokenType
  = Number Int
  | Ident String
  | Return
  | LParen
  | RParen
  | Plus
  | Minus
  | Slash
  | Asterisk
  | L
  | LEq
  | G
  | GEq
  | Eq
  | Neq
  | Assign
  | Semicolon
  deriving (Eq, Ord)

data Token = Token {tokenType :: TokenType, lineState :: !LineState} deriving (Eq, Ord)

instance Show Token where
  show (Token (Number n) _) = show n
  show (Token (Ident ch) _) = show ch
  show (Token Return _) = "return"
  show (Token LParen _) = "("
  show (Token RParen _) = ")"
  show (Token Plus _) = "+"
  show (Token Minus _) = "-"
  show (Token Slash _) = "/"
  show (Token Asterisk _) = "*"
  show (Token L _) = "<"
  show (Token LEq _) = "<_)="
  show (Token G _) = ">"
  show (Token GEq _) = ">_)="
  show (Token Eq _) = "_)=_)="
  show (Token Neq _) = "!_)="
  show (Token Assign _) = "_)="
  show (Token Semicolon _) = ";"

getLineOfToken :: Token -> String
getLineOfToken = T.unpack . line . lineState

getSourcePosOfToken :: Token -> MP.SourcePos
getSourcePosOfToken = sourcePos . lineState

getLineStatep :: Parser LineState
getLineStatep = do
  ps <- MP.statePosState <$> MP.getParserState
  return $ LineState (MP.pstateInput ps) (MP.pstateSourcePos ps)

whitespace :: Parser ()
whitespace = ($> ()) . MP.many . MP.oneOf $ [' ', '\t', '\n']

reserved :: Parser Token
reserved =
  foldr1 (<|>)
    . fmap (\(t, s) -> (Token t <$ MP.string s) <*> getLineStatep)
    $ [ (LParen, "("),
        (RParen, ")"),
        (Plus, "+"),
        (Minus, "-"),
        (Slash, "/"),
        (Asterisk, "*"),
        (LEq, "<="),
        (L, "<"),
        (GEq, ">="),
        (G, ">"),
        (Eq, "=="),
        (Neq, "!="),
        (Assign, "="),
        (Semicolon, ";"),
        (Return, "return")
      ]

ident :: Parser Token
ident =
  Token <$> i <*> getLineStatep <?> "variable name"
  where
    i = Ident <$> MP.some (MP.satisfy C.isAsciiLower)

nonDigitChar :: Parser Char
nonDigitChar =
  MP.satisfy (\ch -> ch /= '0' && C.isDigit ch) <?> "non-zero digit"

number :: Parser Token
number = Token <$> n <*> getLineStatep
  where
    numberString =
      ((:) <$> nonDigitChar <*> MP.many MP.digitChar) <|> (pure <$> MP.digitChar)
    n = Number . read <$> numberString

token :: Parser Token
token = reserved <|> number <|> ident

tokens :: Parser [Token]
tokens = token `MP.sepEndBy` whitespace <* MP.eof

run :: String -> T.Text -> Either (MP.ParseErrorBundle T.Text V.Void) [Token]
run = MP.parse tokens
