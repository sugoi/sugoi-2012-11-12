{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses,
    OverloadedStrings, RankNTypes, ScopedTypeVariables #-}

module Sugoi.Main where

import           Control.Applicative
import qualified Control.Distributed.Process as CH
import qualified Control.Distributed.Process.Node as CH
import qualified Control.Distributed.Process.Serializable as CH
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Control
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Binary as Bin
import qualified Data.ByteString.Char8 as BSS
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Base64.Lazy as Base64
import           Data.Default (def)
import           Data.Lens
import qualified Database.Curry as DB
import qualified Network.Transport.TCP as NTT
import           System.Environment

import Sugoi.Types

encode64 :: (Bin.Binary a) => a -> BS.ByteString
encode64 = Base64.encode . Bin.encode

decode64 :: (Bin.Binary a) => BS.ByteString -> a
decode64 x = case Base64.decode x of
  Left _ -> error "decode 64 failed"
  Right r -> Bin.decode r


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
            let initState :: NetworkState (Problem Integer Int)
                initState = NetworkState
                          { _runDB = RIB runInBase
                          , _nodeName = "Anthony"
                          }
            CH.runProcess localNode $ State.evalStateT server initState
      _ -> putStrLn "give me host and port"


type NewChan a = CH.Process (CH.SendPort a, CH.ReceivePort a)

server :: forall solver . (CH.Serializable (Question solver), CH.Serializable (Answer solver))
       => State.StateT (NetworkState solver) CH.Process ()
server = do
  (RIB runInBase) <- access runDB
  let trans = liftIO . runInBase . DB.transaction

  (sendSlavePort, recvSlavePort) <- lift $ CH.newChan
  (sendSolPort, recvSolPort)     <- lift $
    (CH.newChan :: NewChan (Solution solver))

  liftIO $ BS.putStrLn $ BS.unwords ["Master waiting at ", encode64 sendSlavePort]

  let question :: Question solver
      question = Bin.decode "foobar"

  slaveP <- lift $ CH.receiveChan recvSlavePort
  lift           $ CH.sendChan slaveP (question,sendSolPort)
  sol <- lift $ CH.receiveChan recvSolPort
  let (que,ans) = sol
      key = BSS.concat $ BS.toChunks $ Bin.encode que

  trans $ do
    DB.insert key (Just ans)
  return ()
