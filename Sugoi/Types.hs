{-# LANGUAGE DeriveDataTypeable, ImpredicativeTypes, KindSignatures, 
    RankNTypes, StandaloneDeriving, TemplateHaskell #-}

module Sugoi.Types where

import           Control.Monad.Trans.Control
import           Data.Data
import           Data.Lens.Template ( makeLenses )
import qualified Database.Curry as DB


data Problem typeQ typeA = Problem
  deriving (Eq, Show)

deriving instance Typeable2 Problem

type family Question p :: *
type family Answer   p :: *

type instance Question (Problem q a)  = q
type instance Answer (Problem q a) = a
type Solution p = (Question p, Answer p)


newtype RIB a = RIB (RunInBase (DB.DBMT (Maybe a) IO) IO)


data NetworkState solver = NetworkState
  { _runDB :: RIB (Answer solver)
  , _nodeName :: String
  }
 
$( makeLenses [''NetworkState])


        