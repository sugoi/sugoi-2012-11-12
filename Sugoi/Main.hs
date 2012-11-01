{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, RankNTypes #-}

module Sugoi.Main where

import           Control.Applicative
import qualified Control.Distributed.Process as CH
import qualified Control.Distributed.Process.Node as CH
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Control
import qualified Control.Monad.Trans.State.Strict as State
import           Data.Default (def)
import qualified Database.Curry as DB
import qualified Database.Curry.Storage as DB
import qualified Network.Transport.TCP as NTT
import           System.Environment

import Sugoi.Types

rtable :: CH.RemoteTable
rtable = CH.initRemoteTable


dbConf :: DB.Config
dbConf = def { DB.configPath = Just "sugoi.db" }


data NodeState = NodeState
  { runDB :: RunInBase (DB.DBMT (Maybe Int) IO) IO
  , nodeName :: String
  }


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
            let initState = NodeState
                            { runDB = runInBase
                            , nodeName = "Anthony"
                            }
            CH.runProcess localNode $ State.evalStateT server initState
      _ -> putStrLn "give me host and port"


server :: State.StateT NodeState CH.Process ()
server = do
  runInBase <- runDB <$> State.get
  let trans = liftIO . runInBase . DB.transaction
  liftIO $ putStrLn "hello this is server"
  (sendQPort, recvQPort) <- lift $ CH.newChan
  (sendAPort, recvAPort) <- lift $ CH.newChan
  forever $ do
    key <- lift $ CH.receiveChan recvQPort
    trans $ do
      maybeVal <- DB.lookup key
      case maybeVal of
        Nothing -> DB.insert key Nothing
        _       -> return ()
    (key,val) <- lift $ CH.receiveChan recvAPort
    trans $ DB.insert key (Just val)
