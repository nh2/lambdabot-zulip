{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Data.Text as T
import           Test.Hspec

import           Web.Zulip.Lambdabot


main :: IO ()
main = hspec $ do

  describe "type inference" $ do

    it "evaluates 1 + 1" $ do
      actual <- evalCommand ":t 1 + 1"
      actual `shouldBe` ResultTypecheckSuccess (TypecheckSuccess "Num a => a")

  describe "expression evaluation" $ do

    it "evaluates 1 + 1" $ do
      actual <- evalCommand "1 + 1"
      actual `shouldBe` ResultEvalSuccess (EvalSuccess "2" "Num a => a")

    it "fails on invalid input `1 + `" $ do
      actual <- evalCommand "1 +"
      actual `shouldSatisfy` \case
        ResultErrors{} -> True
        _ -> False

  describe "result formatting" $ do

    it "shortens long commands" $ do
      result <- evalCommand "[1..100000]"
      let actual = formatResult (FloodProtectionOutputLength 2000) result
      actual `shouldSatisfy` ((< 10000) . T.length)
