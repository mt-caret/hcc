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
  | If
  | Else
  | While
  | For
  | Comma
  | LParen
  | RParen
  | LBrack
  | RBrack
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

instance Show TokenType where
  show (Number n) = show n
  show (Ident ch) = show ch
  show Return = "return"
  show If = "if"
  show Else = "else"
  show While = "while"
  show For = "for"
  show Comma = ","
  show LParen = "("
  show RParen = ")"
  show LBrack = "{"
  show RBrack = "}"
  show Plus = "+"
  show Minus = "-"
  show Slash = "/"
  show Asterisk = "*"
  show L = "<"
  show LEq = "<="
  show G = ">"
  show GEq = ">="
  show Eq = "=="
  show Neq = "!="
  show Assign = "="
  show Semicolon = ";"

instance Show Token where
  show = show . tokenType

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
    . fmap (\t -> (Token t <$ matchShow t) <*> getLineStatep)
    $ [ Comma,
        LParen,
        RParen,
        LBrack,
        RBrack,
        Plus,
        Minus,
        Slash,
        Asterisk,
        LEq,
        L,
        GEq,
        G,
        Eq,
        Neq,
        Assign,
        Semicolon
      ]
  where
    matchShow = MP.string . T.pack . show

ident :: Parser Token
ident =
  Token <$> i <*> getLineStatep <?> "variable name"
  where
    i = do
      str <- MP.some (MP.satisfy C.isAsciiLower)
      return $
        case str of
          "return" -> Return
          "if" -> If
          "else" -> Else
          "while" -> While
          "for" -> For
          _ -> Ident str

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
