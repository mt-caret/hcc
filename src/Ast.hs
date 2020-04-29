module Ast where

data Ast
  = Ident String
  | Block [Ast]
  | Assign String Ast
  | Return Ast
  | If Ast Ast
  | IfElse Ast Ast Ast
  | While Ast Ast
  | For (Maybe Ast) (Maybe Ast) (Maybe Ast) Ast
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
  blockS :: [a] -> a
  assignS :: String -> a -> a
  returnS :: a -> a
  ifS :: a -> a -> a
  ifelseS :: a -> a -> a -> a
  whileS :: a -> a -> a
  forS :: Maybe a -> Maybe a -> Maybe a -> a -> a
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
  blockS = Block
  assignS = Assign
  returnS = Return
  ifS = If
  ifelseS = IfElse
  whileS = While
  forS = For
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

