{-# LANGUAGE OverloadedStrings #-}
module ParsingHelper
    ( queryStartsWithSelect, queryStartsWithInsert, queryStartsWithDot
    )
where

import Data.Text

queryStartsWithSelect :: Text -> Bool
queryStartsWithSelect query = "select" `isPrefixOf` query

queryStartsWithInsert :: Text -> Bool
queryStartsWithInsert query = "insert" `isPrefixOf` query

queryStartsWithDot :: Text -> Bool
queryStartsWithDot query = Data.Text.head query == '.'
