{-# LANGUAGE OverloadedStrings #-}
module ConsoleSpec where

import Interpreter
import Test.Hspec

spec :: Spec
spec = describe "SQL Mini Interpreter" $ do
  it "interprets '.exit' as Exit command" $ do
    interpret ".exit" `shouldBe` Exit
  it "interprets 'select' as Select command" $ do
    interpret "select" `shouldBe` Select
  it "interprets 'select from foo' as Select command" $ do
    interpret "select from foo" `shouldBe` Select
  it "interprets 'insert' as Insert command" $ do
    interpret "insert" `shouldBe` Insert
  it "interprets 'insert foo' as Insert command" $ do
    interpret "insert foo" `shouldBe` Insert
  it "interprets unknown meta command as UnknownMeta command" $ do
    interpret ".foo" `shouldBe` UnknownMeta ".foo"
  it "interprets unknown string  as Unknown command" $ do
    interpret "foo" `shouldBe` Unknown "foo"
