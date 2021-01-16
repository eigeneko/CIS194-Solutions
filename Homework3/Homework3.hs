-- ========================================================================== --
--                                 Excercise 1                                --
-- ========================================================================== --

{- 
skips "ABCD" == ["ABCD", "BD", "C", "D"]
skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
skips [1] == [[1]]
skips [True,False] == [[True,False], [False]]
skips [] == []
-}

-- 模式匹配保证了xs不为[]
-- 注意 takeEvery 0 "ABCD" 会得到 "AAAAAAAAA...."
everyN :: Int -> [a] -> [a]
everyN n [] = []
everyN n xs = head xs : everyN n (drop n xs)

takeEvery :: Int -> [a] -> [a]
takeEvery n = everyN n . drop (n-1)

skips :: [a] -> [[a]]
skips [] = []
skips x = [takeEvery n x | n <- takeWhile (not . null . (`takeEvery` x)) [1..]]

{-
如果这样写, 当drop (n-1) xs 为[]时会因为 head [] 报错
takeEvery :: Int -> [a] -> [a]
takeEvery _ [] = []
takeEvery n xs = head (drop (n-1) xs) : takeEvery n (drop n xs)
-}

{-
[x | x <- [1..], x < 1000]
skips x = [takeEvery n x | n <- [1..], not (null (takeEvery n x))]
这样写永远不会停下来，list comprehension的条件相当于filter
not (null (takeEvery n x)) 只用来判断元素是否放进list，而不是条件不满足时立即退出
要想某个条件不满足时跳出应该用takeWhile
 

如何把takeEvery柯里化成接受第一个参数的不全函数
通常我们都是 map (+2) 这样，接受的是一个list，如果想对于确定的list应用任何函数，应该用中缀形式写成 let x = `map` [1,2,3]
-}

-- -------------------------------------------------------------------------- --
--                                 Excercise 2                                --
-- -------------------------------------------------------------------------- --

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [x] = []
localMaxima (x:xs)
    | length xs == 1 = []
    | x <= y && y >=z = y : localMaxima xs
    | otherwise = localMaxima xs
    where y = head xs
          z = head $ drop 1 xs

-- -------------------------------------------------------------------------- --
--                                 Excercise 3                                --
-- -------------------------------------------------------------------------- --

-- draw from topest to lowest, started from length of xs
histogram :: [Int] -> String
histogram xs = drawLines (length xs) xs

-- draw lines of histogram
drawLines :: Int -> [Int] -> String
drawLines row xs
    | row <= 1 = "==========\n0123456789\n"
    | otherwise = drawOneline (row-1) xs ++ drawLines (row-1) xs

-- draw each line of histgram
drawOneline :: Int -> [Int] -> String
drawOneline row xs
    | '*' `elem` str = str
    | otherwise = ""
    where str = map (drawOneChar row xs) [0..9] ++ "\n"
    
-- draw a single char of each line
drawOneChar :: Int -> [Int] -> Int -> Char
drawOneChar row xs number
    | row <= howManyTimes number xs = '*'
    | otherwise = ' '

-- cout how many times an element appreas in a list
howManyTimes :: a -> [a] -> Int
howManyTimes n [] = 0
howManyTimes n (x:xs)
    | n == x = 1 + howManyTimes n xs
    | otherwise = howManyTimes n xs

