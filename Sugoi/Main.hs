{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}

module Sugoi.Main where

import qualified Control.Distributed.Process as CH
import qualified Control.Distributed.Process.Node as CH
import qualified Control.Distributed.Process.Internal.Types as CH
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Control
import           Data.Default (def)
import qualified Database.Curry as DB
import qualified Database.Curry.Storage as DB
import qualified Network.Transport as NT
import qualified Network.Transport.TCP as NTT
import           System.Environment


rtable :: CH.RemoteTable
rtable = CH.initRemoteTable


dbConf :: DB.Config
dbConf = def { DB.configPath = Just "sugoi.db" }


defaultMain :: IO ()
defaultMain = do
  argv <- getArgs
  case argv of
    [host,port] -> do
      ret <- NTT.createTransport host port NTT.defaultTCPParameters
      case ret of
        Left err -> print err
        Right transport -> do
          localNode <- CH.newLocalNode transport rtable
          CH.runProcess localNode server



server :: CH.Process ()
server = do
  liftIO $ putStrLn "hello this is server"
--   DB.runDBMT dbConf $ do
--     DB.transaction $ DB.insert "foo" (65::Int)
--   DB.runDBMT dbConf $ do
--     DB.transaction $ DB.insert "bar" (67::Int)
--   DB.runDBMT dbConf $ do
--     DB.transaction $ DB.insert "baz" (68::Int)
--     DB.saveToFile
