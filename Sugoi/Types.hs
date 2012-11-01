{-# LANGUAGE DeriveDataTypeable, KindSignatures, StandaloneDeriving #-}
module Sugoi.Types where

import          Data.Data


data Problem a b = Problem
  deriving (Eq, Show)

deriving instance Typeable2 Problem

class ProblemClass p where
  type Input p  :: *
  type Output p :: *
  

instance ProblemClass (Problem a b) where
  type Input (Problem a b)  = a
  type Output (Problem a b) = b

 