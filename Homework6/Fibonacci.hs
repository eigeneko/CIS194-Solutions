{-# LANGUAGE ParallelListComp #-}

-- -------------------------------------------------------------------------- --
--                                  Exercise1                                 --
-- -------------------------------------------------------------------------- --

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

fibs1 :: [Integer]
fibs1 = [fib n | n <- [0..]]

-- ========================================================================== --
--                                 Excercise2                                 --
-- ========================================================================== --

fibs2 :: [Integer]
fibs2 = [0, 1] ++ [fibs2 !! (n-1) + fibs2 !! (n-2) | n <- [2..]]

fibs4 :: [Integer]
fibs4 = 0 : 1 : zipWith (+) fibs4 (tail fibs4)

fibs3 :: [Integer]
fibs3 = 0 : 1 : [x + y | x <- fibs3 | y <- tail fibs3] -- need ParallelListComp

-- ========================================================================== --
--                                  Exercise3                                 --
-- ========================================================================== --

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- ========================================================================== --
--                                  Exercise4                                 --
-- ========================================================================== --

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- ========================================================================== --
--                                  Exercise5                                 --
-- ========================================================================== --

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = genRuler 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

genRuler :: Integer -> Stream Integer
genRuler x = interleaveStreams (streamRepeat 0) (genRuler (x+1))