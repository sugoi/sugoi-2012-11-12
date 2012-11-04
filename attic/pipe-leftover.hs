{-# LANGUAGE RankNTypes #-}


import Control.Monad
import Control.Monad.Trans
import Control.Proxy
import Control.Proxy.Prelude.Base
import Data.Monoid

ints :: () -> Server () Integer IO ()
ints () = fromListS [1..10] ()

infix 6 `foldMap`

foldMap :: (a' -> Proxy b' b () c m r) 
        -> (Monoid mo => c -> mo)
        -> (a' -> Proxy b' b () C m mo)

foldMap k f a = undefined


main :: IO ()
main = do
  Sum x <- runSession $ ints >-> mapD (*10) `foldMap` Sum
  print x