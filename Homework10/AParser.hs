{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import Control.Applicative
import Data.Char (isDigit, isNumber, isUpper)
import Data.Maybe

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing -- fail on the empty input
    f (x : xs) -- check if x satisfies the predicate
    -- if so, return x along with the remainder
    -- of the input (that is, xs)
      | p x = Just (x, xs)
      | otherwise = Nothing -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (read ns, rest)
      where
        (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- ========================================================================== --
--                                  Exercise1                                 --
-- ========================================================================== --

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Functor Parser where
  fmap f (Parser fa) = Parser (fmap (first f) . fa)

-- ========================================================================== --
--                                  Exercise2                                 --
-- ========================================================================== --
{-
type Name = String
data Employee = Emp { name :: Name, phone :: String }

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<*>) :: Applicative f => f (a -> b) -> f a -> f b

parseName :: Parser Name
parsePhone :: Parser String
Emp :: Name -> String -> Employee

Emp <$> parserName
(Name -> String -> Employee) -> Parser Name -> Parser (String -> Employee)

(Emp <$> parseName) <*> parsePhone
Parser (String -> Employee) -> Parser String -> Parser Employee

Emp <$> parseName <*> parsePhone :: Parser Employee
-}

-- Parser a, a can be either type, Int, String, or even a funciton !

-- Try to use fst,snd,maybe,fromJust, very ugly !
instance Applicative Parser where
  pure a = Parser (\x -> Just (a, x))

  (Parser f1) <*> (Parser f2) = Parser f3
    where
      f3 x
        | isNothing r1 || isNothing r2 = Nothing
        | otherwise = Just (fromJust a1, a2)
        where
          r1 = f1 x
          r2 = f2 $ maybe "" snd r1
          a1 = fmap fst r1 <*> fmap fst r2
          a2 = maybe "" snd r2

-- ========================================================================== --
--                                  Exercise3                                 --
-- ========================================================================== --

abParser :: Parser (Char, Char)
abParser =  constructTuple <$> char 'a' <*> char 'b'

constructTuple :: Char -> Char -> (Char, Char)
constructTuple a b = (a, b)

abParser_ :: Parser ()
abParser_ = constructEmptyTuple <$> char 'a' <*> char 'b' 

constructEmptyTuple :: Char -> Char -> ()
constructEmptyTuple _ _ = ()

intPair :: Parser [Integer]
intPair = constructTwoNumbers <$> posInt <*> char ' ' <*> posInt

constructTwoNumbers :: Integer -> Char -> Integer -> [Integer]
constructTwoNumbers a _ b = [a, b]

-- ========================================================================== --
--                                  Exercise4                                 --
-- ========================================================================== --

instance Alternative Parser where
  empty = Parser (const Nothing)

  p1 <|> p2 = Parser f
    where f x = runParser p1 x <|> runParser p2 x

-- ========================================================================== --
--                                  Exercise5                                 --
-- ========================================================================== --
-- take hlint suggestion replace :
-- intOrUppercase = const () <$ (satisfy isNumber <|> satisfy isUpper)
intOrUppercase :: Parser ()
intOrUppercase = () <$ (satisfy isNumber <|> satisfy isUpper)
