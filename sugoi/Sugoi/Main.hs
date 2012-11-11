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
import           Data.Conduit
import           Data.Default (def)
import           Data.IORef
import           Data.Lens
import qualified Database.Curry as DB
import qualified Network.Transport.TCP as NTT
import           System.Environment
import           System.IO

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


masterMain :: forall problem. (CH.Serializable (Question problem), CH.Serializable (Answer problem))
           => problem -> IO ()
masterMain _ = do
  DB.runDBMT dbConf $ liftBaseWith $ \runInBase -> do
    argv <- getArgs
    case argv of
      [host,port] -> do
        ret <- NTT.createTransport host port NTT.defaultTCPParameters
        case ret of
          Left err -> print err
          Right transport -> do
            localNode <- CH.newLocalNode transport rtable
            let initState :: ServerState problem
                initState = ServerState
                          { _runDB = RIB runInBase
                          }
            CH.runProcess localNode $ State.evalStateT masterProcess initState
      _ -> putStrLn "give me host and port"


type NewChan a = CH.Process (CH.SendPort a, CH.ReceivePort a)

masterProcess :: forall problem . (CH.Serializable (Question problem), CH.Serializable (Answer problem))
       => State.StateT (ServerState problem) CH.Process ()
masterProcess = do
  (sendWorkerPort, recvWorkerPort) <- lift $
    (CH.newChan :: NewChan
      (CH.SendPort (Question problem, CH.SendPort (Solution problem))))
  (sendSolPort, recvSolPort) <- lift $
    (CH.newChan :: NewChan (Solution problem))

  spawnLocalS $ questionSender recvWorkerPort sendSolPort
  spawnLocalS $ solutionCollector recvSolPort

  liftIO $ do
    BS.hPutStrLn stderr $ BS.unwords ["master recruiting at ", encode64 sendWorkerPort]
    hFlush stderr
  x <- lift $ CH.expect
  return x


  where
    questionSender recvWorkerPort sendSolPort = forever $ do
      q2 <- trans $ do
        ks <- DB.keys
        ks $$ await

      qRef <- liftIO $ newIORef (Nothing :: Maybe (Question problem))

      (RIB runInBase) <- access runDB
      liftIO . runInBase $ do
        q3 <- restoreM q2
        liftIO $ writeIORef qRef $
          (Bin.decode . BS.fromChunks . (:[])) <$> q3

      q4 <- liftIO $ readIORef qRef
      case q4 of
        Nothing -> return ()
        Just question -> do
          workerP <- lift $ CH.receiveChan recvWorkerPort
          lift            $ CH.sendChan workerP (question,sendSolPort)

    solutionCollector recvSolPort = forever $ do
      sol <- lift $ CH.receiveChan recvSolPort
      let (que,ans) = sol
          key = BSS.concat $ BS.toChunks $ Bin.encode que

      trans $ DB.insert key (Just ans)
      return ()

    trans dbm = do
      (RIB runInBase) <- access runDB
      liftIO . runInBase . DB.transaction $ dbm



spawnLocalS :: State.StateT s CH.Process () ->  State.StateT s CH.Process CH.ProcessId
spawnLocalS proc = do
  s <- State.get
  lift $ CH.spawnLocal $ State.evalStateT proc s