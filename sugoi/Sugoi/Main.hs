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
import qualified Data.Conduit.List as DCL
import           Data.Default (def)
import           Data.IORef
import           Data.Lens
import qualified Database.Curry as DB
import qualified Network.Transport.TCP as NTT
import           System.Environment
import           System.IO

import Sugoi.Types

type Sp a = CH.SendPort a
type Rp a = CH.ReceivePort a
type NewChan a = CH.Process (Sp a, Rp a)

encode64 :: (Bin.Binary a) => a -> BS.ByteString
encode64 = Base64.encode . Bin.encode

decode64 :: (Bin.Binary a) => BS.ByteString -> a
decode64 x = case Base64.decode x of
  Left _ -> error "decode 64 failed"
  Right r -> Bin.decode r


toLazyBS :: BSS.ByteString -> BS.ByteString
toLazyBS = BS.fromChunks . (:[])

toStrictBS :: BS.ByteString -> BSS.ByteString
toStrictBS = BSS.concat . BS.toChunks

rtable :: CH.RemoteTable
rtable = CH.initRemoteTable


dbConf :: DB.Config
dbConf = def { DB.configPath = Just "main.db" }


runTCPProcess :: CH.Process () -> IO ()
runTCPProcess proc = do
  argv <- getArgs
  case argv of
    (host:port:_) -> do
      ret <- NTT.createTransport host port NTT.defaultTCPParameters
      case ret of
        Left err -> print err
        Right transport -> do
          localNode <- CH.newLocalNode transport rtable
          CH.runProcess localNode proc
    _ -> putStrLn "give me host and port"

runTCPProcess1 :: (BS.ByteString -> CH.Process ()) -> IO ()
runTCPProcess1 proc = do
  argv <- getArgs
  case argv of
    (_:_:addr:_) -> runTCPProcess $ proc (BS.pack addr)
    _ -> putStrLn "give me host, port and addr"

masterMain :: forall problem. (CH.Serializable (Question problem), CH.Serializable (Answer problem))
           => problem -> IO ()
masterMain _ = do
  DB.runDBMT dbConf $ liftBaseWith $ \runInBase ->
    let initState :: ServerState problem
        initState = ServerState
                    { _runDB = RIB runInBase
                    }
     in runTCPProcess $ State.evalStateT masterProcess initState


masterProcess :: forall problem . (CH.Serializable (Question problem), CH.Serializable (Answer problem))
       => State.StateT (ServerState problem) CH.Process ()
masterProcess = do
  (sendQuestionPort, recvQuestionPort) <- lift $
    (CH.newChan :: NewChan (Question problem))
  (sendWorkerPort, recvWorkerPort) <- lift $
    (CH.newChan :: NewChan
      (Sp (Question problem, Sp (Solution problem))))
  (sendSolPort, recvSolPort) <- lift $
    (CH.newChan :: NewChan (Solution problem))

  spawnLocalS $ questionRegisterer recvQuestionPort
  spawnLocalS $ questionSender recvWorkerPort sendSolPort
  spawnLocalS $ solutionCollector recvSolPort

  liftIO $ do
    BS.writeFile "master.conf" $ BS.unlines
      ["master recruiting at ", encode64 sendWorkerPort,
       "collecting questions at ", encode64 sendQuestionPort
       ]
    hFlush stderr
  x <- lift $ CH.expect
  return x


  where
    questionSender recvWorkerPort sendSolPort = forever $ do
      q2 <- trans $ do
        ks <- DB.keys
        let f q1 = do
              mma <- DB.lookup q1
              return $ case mma of
                Nothing -- the question is not found in DB!
                  -> Nothing
                Just Nothing -- the question is in DB but no answer yet
                  -> Just q1
                Just (Just _) -- already solved
                  -> Nothing
        ks >+> DCL.mapMaybeM f $$ await

      qRef <- liftIO $ newIORef (Nothing :: Maybe (Question problem))

      (RIB runInBase) <- access runDB
      liftIO . runInBase $ do
        q3 <- restoreM q2
        liftIO $ writeIORef qRef $
          (Bin.decode . toLazyBS) <$> q3

      q4 <- liftIO $ readIORef qRef
      case q4 of
        Nothing -> return ()
        Just question -> do
          workerP <- lift $ CH.receiveChan recvWorkerPort
          lift            $ CH.sendChan workerP (question,sendSolPort)

    solutionCollector recvSolPort = forever $ do
      sol <- lift $ CH.receiveChan recvSolPort
      let (que,ans) = sol
          key = toStrictBS $ Bin.encode que

      trans $ DB.insert key (Just ans)
      return ()

    trans dbm = do
      (RIB runInBase) <- access runDB
      liftIO . runInBase . DB.transaction $ dbm

    questionRegisterer recvQPort = forever $ do
      q <- lift $ CH.receiveChan recvQPort
      trans $ DB.insert (toStrictBS $ Bin.encode q) (Nothing)


spawnLocalS :: State.StateT s CH.Process () ->  State.StateT s CH.Process CH.ProcessId
spawnLocalS proc = do
  s <- State.get
  lift $ CH.spawnLocal $ State.evalStateT proc s

workerMain :: forall problem . (CH.Serializable (Question problem), CH.Serializable (Answer problem))
       => problem
       -> (Question problem -> IO (Answer problem))
       -> IO ()
workerMain _ solveIO = runTCPProcess1 $ \addr -> do
  let sendWorkerPort :: Sp (Sp (Question problem, Sp (Solution problem)))
      sendWorkerPort = Bin.decode addr
  (sendPort1, recvPort1) <- CH.newChan :: NewChan (Question problem, Sp (Solution problem))
  forever $ do
    CH.sendChan sendWorkerPort sendPort1
    pair1 <- CH.receiveChan recvPort1
    let question :: Question problem
        sendBackPort :: Sp (Solution problem)
        (question, sendBackPort) = pair1
    answer <- liftIO $ solveIO question
    CH.sendChan sendBackPort (question, answer)

questionerMain :: forall problem . (CH.Serializable (Question problem), CH.Serializable (Answer problem))
       => problem
       -> [Question problem]
       -> IO ()
questionerMain _ qs = runTCPProcess1 $ \addr -> do
  let sendQuestionPort :: Sp (Question problem)
      sendQuestionPort = Bin.decode addr
  forM_ qs $ CH.sendChan sendQuestionPort