module FirstAndSecond where

import Data.List (sort, splitAt)

order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (a, b, c) = (x, y, z)
                 where
                   [x, y, z] = sort [a, b, c]

smartReplicate :: [Int] -> [Int]
smartReplicate = foldr (\x done -> replicate x x ++ done) []

contains :: Eq a => a -> [[a]] -> [[a]]
contains x = filter (elem x)

stringSum :: String -> Int
stringSum = sum . map read . words

removeAt :: Int -> [a] -> ([a], Maybe a)
removeAt i list = (left ++ right, ret)
                where
                  (left, r)    = splitAt i list
                  (right, ret) = removeFst r
                  removeFst :: [a] -> ([a], Maybe a)
                  removeFst []     = ([], Nothing)
                  removeFst (x:xs) = (xs, Just x)

mergeSort :: Ord a => [a] -> [a]
mergeSort [x]  = [x]
mergeSort list = merge (mergeSort left) (mergeSort right)
               where
                 (left, right) = splitAt divsize list
                 divsize       = div (length list) 2
                 merge :: Ord a => [a] -> [a] -> [a]
                 merge [] b          = b
                 merge a []          = a
                 merge a@(x:xs) b@(y:ys)
                                 | x < y     = x:merge xs b
                                 | otherwise = y:merge a ys


--TESTS--
checkOrder :: Bool
checkOrder = order3 (3, 2, 1) == (1, 2, 3) && order3 (3, 1, 2) == (1, 2, 3)

checkSmartReplicate :: Bool
checkSmartReplicate = smartReplicate [3, 2, 1] == [3, 3, 3, 2, 2, 1]

checkContains :: Bool
checkContains = contains 'a' ["abacaba", "dbc", "", "jbije", "a"] == ["abacaba", "a"]

checkStringSum :: Bool
checkStringSum = stringSum " 1\t\n 2 5 10 " == 18

checkRemoveAt :: Bool
checkRemoveAt = removeAt 5 [1, 2, 3, 4, 5, 6] == ([1, 2, 3, 4, 5], Just 6) &&
                removeAt 6 [1, 2, 3, 4, 5, 6] == ([1, 2, 3, 4, 5, 6], Nothing)

checkMergeSort :: Bool
checkMergeSort = mergeSort [5, 1, 2, 10, 3, 4, -5] == [-5, 1, 2, 3, 4, 5, 10]

firstTests :: [Bool]
firstTests = [checkOrder, checkSmartReplicate, checkContains, checkStringSum, checkRemoveAt,
              checkMergeSort]

checkFirst :: [Int]
checkFirst = map fst $ filter (not . snd) $ zip [1..] firstTests
