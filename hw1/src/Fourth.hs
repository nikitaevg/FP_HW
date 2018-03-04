{-# LANGUAGE InstanceSigs #-}
module Fourth where

import Third (Tree (..))

data Pair a = Pair a a deriving (Show)
data NonEmpty a = a :| [a] deriving (Show)

fromList :: [a] -> NonEmpty a
fromList (x:xs) = x:|xs
fromList _      = undefined

toList :: NonEmpty a -> [a]
toList (x:|xs) = x:xs

instance Foldable Pair where
    foldMap :: Monoid m => (a -> m) -> Pair a -> m
    foldMap f (Pair x y) = f x `mappend` f y
    foldr :: (a -> b -> b) -> b -> Pair a -> b
    foldr f acc (Pair x y) = f x $ f y acc

instance Foldable NonEmpty where
    foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
    foldMap f (x :| xs) = f x `mappend` foldMap f xs
    foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
    foldr f acc (x :| xs) = f x $ foldr f acc xs

instance Foldable Tree where
    foldMap :: Monoid m => (a -> m) -> Tree a -> m
    foldMap _ Nil = mempty
    foldMap f (Node list left right) = foldMap f left `mappend`
                                       foldMap f list `mappend`
                                       foldMap f right

splitOn :: (Eq a) => a -> [a] -> NonEmpty [a]
splitOn spl = foldr add (fromList [[]])
            where
              add ch list@(x:|xs)
                            | ch == spl = fromList ([]:x:xs)
                            | otherwise = fromList ((ch:x):xs)

joinWith :: (Eq a) => a -> [[a]] -> [a]
joinWith ch = foldr1 (\str acc -> str `mappend` (ch:acc))

--TESTS--

checkPair :: Bool
checkPair = foldr (++) "" (Pair "aba" "caba") == "abacaba"

checkNonEmpty :: Bool
checkNonEmpty = foldr (+) 5 (4 :| [1,2,3,4]) == 19

checkTree :: Bool
checkTree = foldr (:) [5,6,7,8] (fromList [1,2,3,4]) == [1,2,3,4,5,6,7,8]

checkSplitOn :: Bool
checkSplitOn = let str = " aba cabadab acab" in joinWith 'a' (toList $ splitOn 'a' str) == str

fourthTests :: [Bool]
fourthTests = [checkPair, checkNonEmpty, checkTree, checkSplitOn]

checkFourth :: [Int]
checkFourth = map fst $ filter (not . snd) $ zip [1..] fourthTests
