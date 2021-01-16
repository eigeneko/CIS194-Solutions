-- ========================================================================== --
--                                 Exercies 1                                 --
-- ========================================================================== --

fun1 :: [Integer] -> Integer
fun1 xs = foldl (*) 1 $ map (`subtract` 2) $ filter even xs

-- (/=1) can't stop fun2 0
-- (>=1) can't stop fun2 1
fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (>1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)
 
-- ========================================================================== --
--                                 Excercise 2                                --
-- ========================================================================== --

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree [] = error "Empty Tree!"
foldTree xs = foldr insertNode Leaf xs

-- manually define depth of Leaf to -1
-- because the node don't exist
-- then we can compare the depth of Leaf with (Node 0 Leaf x Leaf)
depth :: Tree a -> Integer
depth Leaf = (-1)
depth (Node dep _ _ _) = dep

insertNode :: a -> Tree a -> Tree a
insertNode x Leaf = Node 0 Leaf x Leaf
insertNode x tree@(Node dep leftTree value rightTree)
    | depth leftTree <= depth rightTree = Node (max (depth newLeft) (depth rightTree) + 1) newLeft value rightTree
    | otherwise = Node (max (depth leftTree) (depth newRight) + 1) leftTree value newRight
    where newLeft = insertNode x leftTree
          newRight = insertNode x rightTree

-- ========================================================================== --
--                                 Excercise 3                                --
-- ========================================================================== --

xor :: [Bool] -> Bool
xor = odd . foldr (\x n -> if x == True then n + 1 else n) 0

-- using ' rather than `
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x ys -> f x : ys) [] xs


-- ========================================================================== --
--                                 Excercise 4                                --
-- ========================================================================== --

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) . (*2)) $ filter (not . (`elem` remove)) [1..n]
            where remove = takeWhile (<=n) [i + j + 2 * i * j | i <- [1..n], j <- [1..n], i <= j]