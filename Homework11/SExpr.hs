{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

-- pure [] :: Applicative f => f [a]
-- 注意顺序，写成 oneOrMore parser <|> pure [] 的话, pure []在前直接返回了空列表，不会正确解析
-- 都是自己想出来的，没看答案
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore parser = oneOrMore parser <|> pure []  

oneOrMore :: Parser a -> Parser [a]
oneOrMore parser = (:) <$> parser <*> zeroOrMore parser

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum) 

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

leftParenth :: Parser Char
leftParenth = char '('

rightParenth :: Parser Char
rightParenth = char ')'

parseAtom :: Parser Atom
parseAtom = N <$> posInt <|> I <$> ident

parseSExpr :: Parser SExpr
parseSExpr = spaces *> ((A <$> parseAtom) <|> (Comb <$> (leftParenth *> oneOrMore parseSExpr <* rightParenth))) <* spaces

a = runParser parseSExpr "5"
b = runParser parseSExpr "foo3"
c = runParser parseSExpr "(bar (foo) 3 5 874)"
d = runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)"
e = runParser parseSExpr "( lots of ( spaces in ) this ( one ) )"