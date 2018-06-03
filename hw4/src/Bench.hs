module Bench where

import           Criterion.Main
import           Criterion.Types
import qualified Data.List as List      

nubFoldl :: Ord a => [a] -> [a]
nubFoldl = foldl (\acc x -> if x `elem` acc then acc else x:acc) []

nubFoldr :: Ord a => [a] -> [a]
nubFoldr = foldr (\x acc -> if x `elem` acc then acc else x:acc) []

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

nubSorted :: Ord a => [a] -> [a]
nubSorted (x:xs) = if x == head xs then xs else x:xs
nubSorted x = x

nubMergeSort :: Ord a => [a] -> [a]
nubMergeSort = nubSorted . mergeSort

main :: IO ()
main = defaultMainWith (defaultConfig {reportFile = Just "benchmark.html"
                                      , timeLimit = 40}) $
    let
        list1, list2, list3, list4 :: [Int]
        list1 = [-100..100] ++ [-200..200]
        list2 = [1..60]
        list3 = [1..50] ++ [1..50]
        list4 = replicate 50 1
    in
    [ bgroup "half repeated"
        [ bench "foldl" $ nf nubFoldl list1
        , bench "foldr" $ nf nubFoldr list1
        , bench "mergeSort" $ nf nubMergeSort list1
        , bench "usual" $ nf List.nub list1
        ],
      bgroup "unique up"
        [ bench "foldl" $ nf nubFoldl list2
        , bench "foldr" $ nf nubFoldr list2
        , bench "mergeSort" $ nf nubMergeSort list2
        , bench "usual" $ nf List.nub list2
        ],
      bgroup "100 x 2"
        [ bench "foldl" $ nf nubFoldl list3
        , bench "foldr" $ nf nubFoldr list3
        , bench "mergeSort" $ nf nubMergeSort list3
        , bench "usual" $ nf List.nub list3
        ],
      bgroup "111111111...1"
        [ bench "foldl" $ nf nubFoldl list4
        , bench "foldr" $ nf nubFoldr list4
        , bench "mergeSort" $ nf nubMergeSort list4
        , bench "usual" $ nf List.nub list4
        ]
    ]

