module Main where

import Test.Tasty (defaultMain, testGroup)

import Spec (hspecTestTree)

main :: IO ()
main = hspecTestTree >>= \unitTests ->
       let allTests = testGroup "Tests" [unitTests]
       in defaultMain allTests
