module Tokenizer
  ( run
  , Token(..)
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

data Token
  = Reserved T.Text
  | Number Int
  deriving Show

whitespace :: Parser ()
whitespace = ($> ()) . MP.many . MP.oneOf $ [' ', '\t', '\n']

reserved :: Parser Token
reserved = Reserved . T.singleton <$> MP.oneOf ['+', '-']

nonDigitChar :: Parser Char
nonDigitChar =
  MP.satisfy (\ch -> ch /= '0' && C.isDigit ch) <?> "non-zero digit"

number :: Parser Token
number = Number . read <$> numberString
  where numberString = (:) <$> nonDigitChar <*> MP.many MP.digitChar

token :: Parser Token
token = reserved <|> number

tokens :: Parser [Token]
tokens = token `MP.sepEndBy` whitespace <* MP.eof

run :: String -> T.Text -> Either (MP.ParseErrorBundle T.Text V.Void) [Token]
run = MP.parse tokens
