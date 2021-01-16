module JointList where

import Sized
import Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
        deriving (Eq, Show)

-- ========================================================================== --
--                                 Exercise1                                 --
-- ========================================================================== --

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b


tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- ========================================================================== --
--                                  Exercise2                                 --
-- ========================================================================== --

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- index start from 0
-- | Finds the JoinList element at the specified index.
-- | (indexJ i jl) == (jlToList jl !!? i)
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ index (Single _ x)
    | index == 0 = Just x
    | otherwise  = Nothing
indexJ index (Append m left right) 
    | index < 0 || index > listSize = Nothing
    | index < leftSize              = indexJ index left
    | otherwise                     = indexJ (index - leftSize) right
    where listSize = getSize . size $ m
          leftSize = getSize . size . tag $ left

-- | Drops the first *n* elements of *jl*
-- | jlToList (dropJ n jl) == drop n (jlToList jl)
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ num list@(Single _ _)
    | num <= 0  = list
    | otherwise = Empty
dropJ num list@(Append m left right)
    | num <= 0         = list
    | num <= leftSize  = droppedLeft +++ right
    | otherwise        = droppedRight
    where leftSize     = getSize . size . tag $ left
          droppedLeft  = dropJ num left
          droppedRight = dropJ (num - leftSize) right

-- | Returns the first *n* elements of *jl*
-- | jlToList (takeJ n jl) == take n (jlToList jl)
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ num list@(Single _ _)
    | num <= 0        = Empty
    | otherwise       = list
takeJ num list@(Append m left right)
    | num <= 0        = Empty
    | num <= leftSize = takeLeft
    | otherwise = left +++ takeRight
    where leftSize    = getSize . size . tag $ left
          takeLeft    = takeJ num left
          takeRight   = takeJ (num - leftSize) right

-- Just for testing. All identities should hold as descriped in the comments.
-- Example:
-- (indexJ index a) == (jlToList a !!? index) for all indices i
(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0         = Just x
(x:xs) !!? i         = xs !!? (i-1)

x = Single (Size 1) 'a'
y = Single (Size 1) 'b'
z = Append (tag x <> tag y) x y

-- -------------------------------------------------------------------------- --
--                                  Exercise4                                 --
-- -------------------------------------------------------------------------- --

scoreLine :: String -> JoinList Score String
scoreLine ·str = Single (scoreString str) str

-- line and numLines assume each line is count as Size 1, not every character
instance Buffer (JoinList (Score, Size) String) where
    toString = unlines . jlToList

    fromString = foldl (\x char -> x +++ current char) Empty . unlines
            where current char = Single (scoreString char, 1) char

    line = indexJ

    -- can't handle out-of-bounds
    replaceLine n str b = takeJ n b +++ fromString str +++ dropJ (n+1) b

    numLines = getSize . snd . tag

    value = getSize . fst . tag

main = runEditor editor (fromString "test" :: (JoinList (Score, Size) String)

