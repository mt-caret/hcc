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
  | LParen !LineState
  | RParen !LineState
  | Plus !LineState
  | Minus !LineState
  | Slash !LineState
  | Asterisk !LineState
  deriving (Eq, Ord)

instance Show Token where
  show (Number n _) = show n
  show (LParen   _) = "("
  show (RParen   _) = ")"
  show (Plus     _) = "+"
  show (Minus    _) = "-"
  show (Slash    _) = "/"
  show (Asterisk _) = "*"

getLineState :: Token -> LineState
getLineState (Number _ ls) = ls
getLineState (LParen   ls) = ls
getLineState (RParen   ls) = ls
getLineState (Plus     ls) = ls
getLineState (Minus    ls) = ls
getLineState (Slash    ls) = ls
getLineState (Asterisk ls) = ls

getLineOfToken :: Token -> String
getLineOfToken = T.unpack . line . getLineState

getSourcePosOfToken :: Token -> MP.SourcePos
getSourcePosOfToken = sourcePos . getLineState

getLineStatep = do
  ps <- MP.statePosState <$> MP.getParserState
  return $ LineState (MP.pstateInput ps) (MP.pstateSourcePos ps)

whitespace :: Parser ()
whitespace = ($> ()) . MP.many . MP.oneOf $ [' ', '\t', '\n']

reserved :: Parser Token
reserved =
  foldr1 (<|>)
    . fmap (\(t, ch) -> (t <$ MP.char ch) <*> getLineStatep)
    $ [ (LParen  , '(')
      , (RParen  , ')')
      , (Plus    , '+')
      , (Minus   , '-')
      , (Slash   , '/')
      , (Asterisk, '*')
      ]

nonDigitChar :: Parser Char
nonDigitChar =
  MP.satisfy (\ch -> ch /= '0' && C.isDigit ch) <?> "non-zero digit"

number :: Parser Token
number = Number . read <$> numberString <*> getLineStatep
  where numberString = (:) <$> nonDigitChar <*> MP.many MP.digitChar

token :: Parser Token
token = reserved <|> number

tokens :: Parser [Token]
tokens = token `MP.sepEndBy` whitespace <* MP.eof

run :: String -> T.Text -> Either (MP.ParseErrorBundle T.Text V.Void) [Token]
run = MP.parse tokens
