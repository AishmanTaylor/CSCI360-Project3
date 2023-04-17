-- CSCI 360, Spring 2023
-- Project 3: the Quilt language

module Quilt where
import Parsing2
import Data.Bool (bool)

-- | A color is a list of red, green, and blue values between 0.0 - 1.0.
--   For example, [0,0,0] is black, [1,1,1] is white, [0.5, 0, 0.5] is a
--   darkish purple, and so on.
type Color = [Double]

-- | A quilt function produces a Color for any given location.  The
--   parameters are x and y coordinates in the range [-1,1].
type QuiltFun = Double -> Double -> Color

-- | Right now, this function ignores the input and simply produces a
--   blue image.  Obviously, you should make this function more
--   interesting!
evalQuilt :: String -> Either String QuiltFun
evalQuilt s = Right $ \x y -> [0,0,1]

{-# LANGUAGE GADTSyntax #-}
-- General Grammar Guide for Quilts

-- <colorlit> ::= 'red' | 'orange' | 'yellow' | ...
-- <num>      ::= integer or floating-point
-- <coord>    ::= 'x' | 'y'
-- <bool>     ::= 'False' | 'True'

-- <qexp> ::=
--   | <colorlit>
--   | <num>
--   | <coord>
--   | <bool>
--   | '[' <qexp> ',' <qexp> ',' <qexp> ']'
--   | 'if' <qexp> 'then' <qexp> 'else' <qexp>
--   | <uop> <qexp>
--   | <qexp> <bop> <qexp>
--   | 'quilt' <qexp> <qexp> <qexp> <qexp>

-- <uop>        ::= '-' | '!'
-- <bop>        ::= <arith> | <comparison> | <boolean>
-- <arith>      ::= '+' | '-' | '*' | '/'
-- <comparison> ::= '<' | '>' | ...
-- <boolean>    ::= '&&' | '||'

data Quilt where
    Boolean :: Bool -> Quilt
    Number  :: Double -> Quilt
    Color   :: Double -> Double -> Double -> Quilt

parseQuiltAtom :: Parser Quilt
parseQuiltAtom = 
  -- impliment <colorlit>?
  Number <$> double
  -- impliment <coords>?
  <|> Boolean <$> bool
  <|> If <$> (reserved "if" *> parseArith) <*> (reserved "then" *> parseArith) <*> (reserved "else" *> parseArith)


lexer :: TokenParser 
lexer = makeTokenParser $ emptyDef
   { reservedNames = ["let", "in", "False", "True", "if", "then", "else"] }

reserved :: String -> Parser ()
reserved   = getReserved lexer

double :: Parser Double
double    = getDouble lexer