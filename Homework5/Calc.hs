{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import ExprT
import Parser
import StackVM
import Data.Maybe

-- ========================================================================== --
--                                 Excercise1                                 --
-- ========================================================================== --

-- eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20

eval :: ExprT -> Integer
eval (ExprT.Lit x) = x
eval (ExprT.Add x y) = eval x + eval y
eval (ExprT.Mul x y) = eval x * eval y

-- ========================================================================== --
--                                 Excercise2                                 --
-- ========================================================================== --

evalStr :: String -> Maybe Integer
evalStr str 
    | expression == Nothing = Nothing
    | otherwise = Just $ eval $ fromJust expression
    where expression = parseExp ExprT.Lit ExprT.Add ExprT.Mul str
-- Maybe是一个functor，可以用fmap来unwrapMaybe而无需使用fromJust
-- fmap show $ Just 42

-- ========================================================================== --
--                                 Excercise3                                 --
-- ========================================================================== --

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit x = ExprT.Lit x
    add x y = ExprT.Add x y
    mul x y = ExprT.Mul x y

reify :: ExprT -> ExprT
reify = id

-- ========================================================================== --
--                                 Excercise4                                 --
-- ========================================================================== --

instance Expr Integer where
    lit x = x
    add x y = x + y
    mul x y = x * y

instance Expr Bool where
    lit x
        | x <= 0    = False
        | otherwise = True
    add x y = x || y
    mul x y = x && y

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
    lit x = MinMax x
    add (MinMax x) (MinMax y) = MinMax $ max x y
    mul (MinMax x) (MinMax y) = MinMax $ min x y

newtype Mod7   = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit x = Mod7 $ mod x 7
    add (Mod7 x) (Mod7 y) = Mod7 $ mod (x+y) 7
    mul (Mod7 x) (Mod7 y) = Mod7 $ mod (x*y) 7

-- -------------------------------------------------------------------------- --

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testExp' :: Expr a => Maybe a
testExp' = parseExp lit add mul "-7"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax -- why it's Just (MinMax 5) ???
testSat     = testExp :: Maybe Mod7   

testMM'     = testExp' :: Maybe MinMax -- Just (MinMax (-7))
testSat'    = testExp' :: Maybe Mod7   -- Just (Mod7 0)

-- -------------------------------------------------------------------------- --
--                                 Excercise5                                 --
-- -------------------------------------------------------------------------- --

instance Expr Program where
    lit x = [PushI x]
    add x y = x ++ y ++ [StackVM.Add]
    mul x y = x ++ y ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile str = parseExp lit add mul str :: Maybe Program

