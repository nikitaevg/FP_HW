module Prop where

import Hedgehog

import qualified Data.List as List
import First (bin)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genInt:: Gen Int
genInt= Gen.int (Range.linear 0 25)

bin_to_int :: [Int] -> Int
bin_to_int = foldr (\b acc -> b + acc * 2) 0

bins_to_int :: [[Int]] -> [Int]
bins_to_int = map bin_to_int

-- I managed to run this test only this way: stack ghci --test
--                                           :l test/Prop.hs
--                                           check prop_bin

prop_bin :: Property
prop_bin = property $
    forAll genInt >>= \l ->
    let ints = bins_to_int (bin l) in List.sort(ints) === [0..(2^l - 1)]

