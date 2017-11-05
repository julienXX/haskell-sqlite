{-# LANGUAGE OverloadedStrings #-}
module ParsingHelperSpec where

import ParsingHelper
import Test.Hspec

spec :: Spec
spec = describe "Parsing helpers" $ do
    it "returns True if a query starts with 'select'" $ do
      queryStartsWithSelect "select foo" `shouldBe` True
    it "returns False if a query don't start with 'select'" $ do
      queryStartsWithSelect "foo" `shouldBe` False
    it "returns True if a query starts with 'insert'" $ do
      queryStartsWithInsert "insert foo" `shouldBe` True
    it "returns False if a query don't start with 'insert'" $ do
      queryStartsWithInsert "foo" `shouldBe` False
    it "returns True if a query starts with a dot" $ do
      queryStartsWithDot ".tables" `shouldBe` True
    it "returns False if a query don't start with a dot" $ do
      queryStartsWithDot "tables" `shouldBe` False
