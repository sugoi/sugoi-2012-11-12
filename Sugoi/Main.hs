{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, RankNTypes #-}

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
  DB.runDBMT dbConf $ liftBaseWith $ \runInBase -> do
    argv <- getArgs
    case argv of
      [host,port] -> do
        ret <- NTT.createTransport host port NTT.defaultTCPParameters
        case ret of
          Left err -> print err
          Right transport -> do
            localNode <- CH.newLocalNode transport rtable
            CH.runProcess localNode (server runInBase)



server :: (RunInBase (DB.DBMT Int IO) IO) -> CH.Process ()
server runInBase = do
  liftIO $ putStrLn "hello this is server"
  liftIO $ runInBase $ DB.transaction $ DB.insert "alpha" (65::Int)
  liftIO $ putStrLn "hello this is server 2"
  liftIO $ runInBase $ DB.transaction $ DB.insert "beta" (66::Int)
  return ()