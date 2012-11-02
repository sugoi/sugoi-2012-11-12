{-# LANGUAGE DeriveDataTypeable, ImpredicativeTypes, KindSignatures, 
    RankNTypes, StandaloneDeriving, TemplateHaskell #-}

module Sugoi.Types where

import           Control.Monad.Trans.Control
import           Data.Data
import           Data.Lens.Strict
import           Data.Lens.Template ( nameMakeLens )
import qualified Database.Curry as DB

import           Sugoi.Types.Internal ( lensNamingRule )

data Problem a b = Problem
  deriving (Eq, Show)

deriving instance Typeable2 Problem

class ProblemClass p where
  type Input p  :: *
  type Output p :: *
  

instance ProblemClass (Problem a b) where
  type Input (Problem a b)  = a
  type Output (Problem a b) = b

-- type AT =  (RunInBase (DB.DBMT (Maybe Int) IO) IO)

data NetworkState = NetworkState
  { _runDB :: forall a. IO a -> Maybe a
  , _nodeName :: String
  }
 

$( nameMakeLens ''NetworkState lensNamingRule)