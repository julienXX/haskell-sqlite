{-# LANGUAGE OverloadedStrings #-}
module Interpreter
    ( -- * Top-level CLI
      console
      -- * Interpreter
    , Command(..), interpret
      -- * Helpers
    , queryStartsWithSelect, queryStartsWithInsert, queryStartsWithDot
    )
where

import Data.Text
import Data.Monoid((<>))

data Command = Exit
             | Select
             | Insert
             | UnknownMeta Text
             | Unknown Text
  deriving (Eq, Show)

interpret :: Text -> Command
interpret ".exit"  = Exit
interpret query
  | queryStartsWithSelect query = Select
  | queryStartsWithInsert query = Insert
  | queryStartsWithDot    query = UnknownMeta query
interpret unknown  = Unknown unknown

queryStartsWithSelect :: Text -> Bool
queryStartsWithSelect query = "select" `isPrefixOf` query

queryStartsWithInsert :: Text -> Bool
queryStartsWithInsert query = "insert" `isPrefixOf` query

queryStartsWithDot :: Text -> Bool
queryStartsWithDot query = Data.Text.head query == '.'

console :: IO ()
console = do
  putStr "> "
  line <- getLine
  let output = interpret $ pack line
  case output of
    Exit -> putStrLn "bye !"
    Select -> do
      putStrLn "This is where we would do a select."
      console
    Insert -> do
      putStrLn "This is where we would do an insert."
      console
    UnknownMeta meta -> do
      putStrLn ("Unrecognized command: " <> unpack meta)
      console
    Unknown com -> do
      putStrLn ("Unrecognized keyword at start of: " <> unpack com)
      console
