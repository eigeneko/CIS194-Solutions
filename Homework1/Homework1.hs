-- ========================================================================== --
--                       Validating Credit Card Numbers                       --
-- ========================================================================== --

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n
    | n < 0 = []
    | otherwise = toDigits (div n 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n 
    | n < 0 = []
    | otherwise = n `mod` 10 : toDigits (div n 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:y)
    | odd (length y) = 2*x : doubleEveryOther y
    | otherwise = x : doubleEveryOther y

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
    | x < 10 = x + sumDigits xs
    | otherwise = (x `div` 10) + (x `mod` 10) + sumDigits xs

validate :: Integer -> Bool
validate n
    | n <= 0 = False
    | otherwise = div (sumDigits $ doubleEveryOther $ toDigits n) 10 == 0


-- ========================================================================== --
--                                 Hanoi Tower                                --
-- ========================================================================== --

type Peg = String
type Move = (Peg, Peg)

-- return a list of moves to be performed to move the stack of discs from first peg to the second.
-- hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
-- 1. move n-1 discs from a to c using b as temporary storage
-- 2. move the top disc from a to b
-- 3. move n-1 discs from c to b using a as temporary storage
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c 
    | n <= 0 = []
    | otherwise = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a