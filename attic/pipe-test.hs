{-# LANGUAGE RankNTypes #-}

import Control.Monad
import Control.Monad.Trans
import Control.Proxy
import Control.Proxy.Pipe
import Control.Proxy.Prelude.Base

ints :: Producer Integer IO ()
ints = forM_ [1..] $ yield

printer :: Consumer Integer IO ()
printer = forever $ await >>= (lift . print)

main :: IO ()
main = runPipe $ ints >+> printer