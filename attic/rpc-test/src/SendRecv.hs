
import qualified Data.ByteString.Base64.Lazy as BS64
import qualified Control.Distributed.Process as DP
import qualified Control.Distributed.Process.Node as DP
import qualified Control.Distributed.Process.Internal.Types as DP
import           Control.Monad (forever)
import qualified Data.Binary as Bin
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Internal as BS
import           Data.Char (isDigit)
import           Data.IORef
import qualified Network.Transport     as NT
import qualified Network.Transport.TCP as NTT
import           System.Environment
import           System.IO


rtable :: DP.RemoteTable
rtable = DP.initRemoteTable

main :: IO ()
main = do
  argv <- getArgs
  case argv of
    ["recv", host, port] -> do
      ret <- NTT.createTransport host port NTT.defaultTCPParameters
      case ret of
        Left err -> print err
        Right transport -> do
          localNode <- DP.newLocalNode transport rtable
          DP.runProcess localNode recvProc
    ["send", host, port, target] -> do
      ret <- NTT.createTransport host port NTT.defaultTCPParameters
      case ret of
        Left err -> print err
        Right transport -> do
          localNode <- DP.newLocalNode transport rtable
          print $ DP.nodeAddress $ DP.localNodeId localNode
          let Right r = BS64.decode $ BS.pack $ target
              pid = Bin.decode $ r
          DP.runProcess localNode (sendProc pid)

recvProc :: DP.Process ()
recvProc = do
  pid <- DP.getSelfPid
  counter <- DP.liftIO $ newIORef (0::Int)
  DP.liftIO $ putStr $ "waiting messages at : "
  DP.liftIO $ BS.putStrLn $ BS64.encode $ Bin.encode pid
  let matchStr str = DP.liftIO $ do
        putStrLn $ "received str : " ++ str
        modifyIORef counter ((length str)+)
      matchInt i = do
        DP.liftIO $ putStrLn $ "received int : " ++ show (i::Int)
          ++ " x2 = " ++ show (2*i)
      matchPid otherPid = do
        DP.liftIO $ putStrLn $ "received pid : " ++ show otherPid
        val <- DP.liftIO $ readIORef counter
        DP.send otherPid $ "you have sent " ++ show val ++ " chars so far"
  forever $ DP.receiveWait [DP.match matchStr,
                            DP.match matchInt,
                            DP.match matchPid]

sendProc :: DP.ProcessId -> DP.Process ()
sendProc pid = forever $ do
  DP.liftIO $ putStr "> "
  DP.liftIO $ hFlush stdout
  str   <- DP.liftIO $ getLine
  myPid <- DP.getSelfPid
  case str of
    "?"                 -> do
                           DP.send pid myPid
                           response <- DP.expect
                           DP.liftIO $ putStrLn response
    _ | all isDigit str -> DP.send pid (read str :: Int)
    _                   -> DP.send pid str
