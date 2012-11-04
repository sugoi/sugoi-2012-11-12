{-# LANGUAGE RankNTypes #-}

import Control.Monad
import Control.Monad.Trans
import Control.Proxy
import Control.Proxy.Prelude.Base

ints :: () -> Server () Integer IO ()
ints () = fromListS [1..] ()

printer :: () -> Client () Integer IO ()
printer () = forever $ await >>= (lift . print)

main :: IO ()
main = runSession $ ints >-> filterD even >-> takeB_ 10 >-> printer