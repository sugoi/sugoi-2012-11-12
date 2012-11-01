{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, RankNTypes #-}

module Sugoi.Main where

import qualified Control.Distributed.Process as CH
import qualified Control.Distributed.Process.Node as CH
import qualified Control.Distributed.Process.Internal.Types as CH
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Control
import           Data.Default (def)
import qualified Database.Curry as DB
import qualified Database.Curry.Storage as DB
import qualified Network.Transport as NT
import qualified Network.Transport.TCP as NTT
import           System.Environment

import Sugoi.Types

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



server :: (RunInBase (DB.DBMT (Maybe Int) IO) IO) -> CH.Process ()
server runInBase = do
  let trans = liftIO . runInBase . DB.transaction
  liftIO $ putStrLn "hello this is server"
  (sendQPort, recvQPort) <- CH.newChan
  (sendAPort, recvAPort) <- CH.newChan
  forever $ do
    key <- CH.receiveChan recvQPort
    trans $ do
      maybeVal <- DB.lookup key
      case maybeVal of
        Nothing -> DB.insert key Nothing
        _       -> return ()
    (key,val) <- CH.receiveChan recvAPort
    trans $ DB.insert key (Just val)
