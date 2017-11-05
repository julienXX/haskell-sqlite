{-# LANGUAGE OverloadedStrings #-}
module Interpreter
    ( -- * Top-level CLI
      console
      -- * Interpreter
    , Command(..), interpret
    )
where

import ParsingHelper
import System.IO
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

outputResponse :: String -> IO()
outputResponse text = putStrLn text >> console

console :: IO ()
console = do
  putStr "> "
  hFlush stdout
  line <- getLine
  let output = interpret $ pack line
  case output of
    Exit             -> putStrLn "bye !"
    Select           -> outputResponse "This is where we would do a select."
    Insert           -> outputResponse "This is where we would do an insert."
    UnknownMeta meta -> outputResponse ("Unrecognized command: " <> unpack meta)
    Unknown com      -> outputResponse ("Unrecognized keyword at start of: " <> unpack com)
