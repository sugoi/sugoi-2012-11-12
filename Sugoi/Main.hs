{-# LANGUAGE OverloadedStrings #-}

module Sugoi.Main where

import Data.Default (def)
import qualified Database.Curry as DB
import qualified Database.Curry.Storage as DB



dbConf :: DB.Config
dbConf = def { DB.configPath = Just "sugoi.db" }

defaultMain :: IO ()
defaultMain = do
  putStrLn "hello this is server"
  DB.runDBMT dbConf $ do
    DB.transaction $ DB.insert "foo" (123::Int)
    DB.saveToFile
