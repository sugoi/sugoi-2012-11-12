{-# LANGUAGE DeriveDataTypeable, ImpredicativeTypes, KindSignatures, 
    RankNTypes, StandaloneDeriving, TemplateHaskell #-}

module Sugoi.Types where

import           Control.Monad.Trans.Control
import           Data.Data
import           Data.Lens.Template ( makeLenses )
import qualified Database.Curry as DB


data Solver typeQ typeA = Solver
  deriving (Eq, Show)

deriving instance Typeable2 Solver

type family Question p :: *
type family Answer   p :: *
type family Solution p :: *

type instance Question (Solver q a)  = q
type instance Answer (Solver q a) = a
type instance Solution (Solver q a) = (q,a)

newtype RIB a = RIB (RunInBase (DB.DBMT (Maybe a) IO) IO)


data NetworkState solver = NetworkState
  { _runDB :: RIB (Answer solver)
  , _nodeName :: String
  }
 
$( makeLenses [''NetworkState])


        