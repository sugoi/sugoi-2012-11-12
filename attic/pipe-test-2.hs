{-# LANGUAGE RankNTypes #-}

import Control.Monad
import Control.Monad.Trans
import Control.Proxy
import Control.Proxy.Prelude.Base

{-

printer ::     Int -> Server Int ()   IO ()
                              A  |
                              |  V
beautyFilter :: Int -> Proxy Int () Int (Maybe Int)   IO ()
                                     A     |
                                     |     V
           improver :: Int -> Proxy Int (Maybe Int) Int ()   IO ()
                                                     A  |
                                                     |  V
                               ints :: () -> Client Int ()   IO ()
                                               A
                                               |
                            runSession inserts () here
-}

ints :: () -> Client Int ()   IO ()
ints () = fromListC [1..] ()


printer x = do
  lift $ putStrLn $ "printing " ++ show x
  respond () >>= printer

beautyFilter :: Int -> Proxy Int () Int (Maybe Int)   IO ()
beautyFilter x
  | x `mod` 8 == 0 = request x >> respond Nothing >>= beautyFilter
  | otherwise      = do
                     lift $ putStrLn $ show x ++ " is too ugly, try again"
                     respond (Just x) >>= beautyFilter

improver :: Int -> Proxy Int (Maybe Int) Int ()   IO ()
improver x = request x >>= cont
  where
    cont Nothing = respond () >>= improver
    cont (Just oldX) = do
                       let newX = oldX * 10
                       lift $ putStrLn $ "how about " ++ show newX
                       request newX >>= cont

main :: IO ()
main = runSession $ printer >-> beautyFilter >-> improver >-> takeB_ 10 >-> ints