{-# LANGUAGE KindSignatures #-}
module Sugoi.Types where

data Problem a b = Problem

class ProblemClass p where
  type Input p  :: *
  type Output p :: *
  

instance ProblemClass (Problem a b) where
  type Input (Problem a b)  = a
  type Output (Problem a b) = b