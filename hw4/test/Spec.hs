module Spec
       ( hspecTestTree
       ) where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Lens (_1, _2, view, set)

hspecTestTree :: IO TestTree
hspecTestTree = testSpec "HW" spec_arithmetic

spec_arithmetic :: Spec
spec_arithmetic = do
  let a = 1 :: Int
  let b = 2 :: Int
  let c = 'c'
  let d = 'd'
  let initial = (a, c)
  describe "Lenzzz" $ do
    it "set _1" $ do
      set _1 b initial `shouldBe` (b, c)
    it "set _2" $ do
      set _2 d initial `shouldBe` (a, d)
    it "view _1" $ do
      view _1 initial `shouldBe` a
    it "view _2" $ do
      view _2 initial `shouldBe` c
