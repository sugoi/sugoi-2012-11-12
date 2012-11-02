{-# LANGUAGE DeriveDataTypeable, ImpredicativeTypes, KindSignatures, 
    RankNTypes, StandaloneDeriving, TemplateHaskell #-}

module Sugoi.Types where

import           Control.Monad.Trans.Control
import           Data.Data
import           Data.Lens.Template ( makeLenses )
import qualified Database.Curry as DB


data Solver a b = Solver
  deriving (Eq, Show)

deriving instance Typeable2 Solver

class SolverClass p where
  type Question p  :: *
  type Answer p :: *
  type Solution p :: *
  

instance SolverClass (Solver a b) where
  type Question (Solver a b)  = a
  type Answer (Solver a b) = b
  type Solution (Solver a b) = (a,b)

newtype RIB = RIB (RunInBase (DB.DBMT (Maybe Int) IO) IO)

data NetworkState = NetworkState
  { _runDB :: RIB
  , _nodeName :: String
  }
 
$( makeLenses [''NetworkState])


        