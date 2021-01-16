{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee
import Data.Tree

-- ========================================================================== --
--                                  Exercise1                                 --
-- ========================================================================== --

glCons :: Employee -> GuestList -> GuestList
glCons employee (GL list fun) = GL (employee : list) (fun + empFun employee)

instance Semigroup GuestList where
    (<>) = add

instance Monoid GuestList where
    mempty = GL [] 0
    mappend = add

add :: GuestList -> GuestList -> GuestList
add (GL list1 fun1) (GL list2 fun2) = GL (list1 ++ list2) (fun1 + fun2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun a b 
    | a >= b = a
    | otherwise = b

-- ========================================================================== --
--                                  Exercise2                                 --
-- ========================================================================== --

treeFold :: (b -> a -> b) -> b -> Tree a -> b
treeFold f init tree = foldl fTree start (subForest tree)
            where fTree = treeFold f
                  start = f init (rootLabel tree)

-- Testing function for treeFold
addE :: Fun -> Employee -> Fun
addE fun employee = fun + empFun employee


-- ========================================================================== --
--                                 Excercise3                                 --
-- ========================================================================== --
-- We sum each subtrees rather than finding maximum of each subtree.
-- Which measn we can invite all the employee in each subtree, not only on subtree.
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss list = (listWithBoss, listWithoutBoss)
    where listWithBoss = foldl add (glCons boss mempty) (fmap snd list)
          listWithoutBoss = foldl add mempty (fmap fst list)

nextLevel' :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel' boss gls = (glCons boss withoutBosses, withBosses)
  where withBosses    = foldMap fst gls
        withoutBosses = foldMap snd gls

-- -------------------------------------------------------------------------- --
--                                  Exercise4                                 --
-- -------------------------------------------------------------------------- --

maxFun :: Tree Employee -> GuestList
maxFun tree = moreFun withBoss withoutBoss
            where results = getGuestList tree
                  withBoss = fst results
                  withoutBoss = snd results

getGuestList :: Tree Employee -> (GuestList, GuestList)
getGuestList tree = nextLevel (rootLabel tree) (map getGuestList $ subForest tree)

-- -------------------------------------------------------------------------- --
--                                  Exercise5                                 --
-- -------------------------------------------------------------------------- --

main :: IO()
main = readFile "company.txt" >>= putStrLn . formatGuestList . maxFun . read

formatGuestList :: GuestList -> String
formatGuestList (GL list fun) = "Total fun: " ++ show fun ++ "\n" ++ unlines employees
    where employees = map (\(Emp {empName = name}) -> name) list