module ASpec
       ( hspecTestTree
       ) where

import Data.Maybe (isNothing)

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, shouldSatisfy, testSpec)

import First (ArithmeticError (..), Expr (..), eval)
import Second (stringSum)
import Third (Parser (..), parseBrackets, parseList, parseNum)

hspecTestTree :: IO TestTree
hspecTestTree = testSpec "HW" spec_arithmetic

spec_arithmetic :: Spec
spec_arithmetic = do
  describe "arithmetic" $ do
    it "simple computation" $ do
      eval (Add (Const 4) (Const 5)) `shouldBe` Right 9
    it "difficult computation" $ do
      eval (Sub (Pow (Const 2) (Add (Const 3) (Div (Const 10) (Const 2)))) (Const 6)) `shouldSatisfy` (==Right 250)
    it "div by zero" $ do
      eval (Div (Const 5) (Const 0)) `shouldBe` Left ZeroDiv
    it "negative power" $ do
      eval (Pow (Const 4) (Const (-4))) `shouldBe` Left NegativePow
  describe "stringsum" $ do
    it "correct" $ do
      stringSum "1         2 3          \t\n4 5" `shouldBe`  Just 15
    it "mistakes" $ do
      stringSum "1 2 d 3 4" `shouldSatisfy` isNothing
    it "empty" $ do
      stringSum "" `shouldBe`  Just 0
  -- since brackets, number and list parsers use ok, eof and etc, no need in tests for them
  describe "brackets" $ do
    it "correct" $ do
      runParser parseBrackets "( ( )   (    )  ) (   )   (())  " `shouldSatisfy` (\x -> (fst <$> x) == Just "(()())()(())")
    it "incorrect" $ do
      runParser parseBrackets "()())()" `shouldSatisfy` (isNothing)
    it "very incorrect" $ do
      runParser parseBrackets "ddd" `shouldSatisfy` (isNothing)
  describe "parseNum" $ do
    it "num" $ do
      runParser parseNum "12345" `shouldBe`  Just (12345, "")
    it "num with sign" $ do
      runParser parseNum "-12345" `shouldBe`  Just (-12345, "")
    it "num with text" $ do
      runParser parseNum "+12345,12" `shouldBe`  Just (12345, ",12")
  describe "parseList" $ do
    it "correct" $ do
      runParser parseList "2, 1,+10  , 3,5,-7, 2" `shouldBe`  Just ([ [1, 10], [5, -7, 2]], "")
    it "incorrect" $ do
      runParser parseList "4, 2, 3" `shouldBe`  Just ([], "4, 2, 3")
    it "correct with text after" $ do
      runParser parseList "5, 1, -2,     3 ,-4 \t ,-5, abacaba" `shouldBe` Just ([[1, -2, 3, -4, -5]], ", abacaba")

