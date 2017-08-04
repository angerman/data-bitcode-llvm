{-# LANGUAGE DeriveGeneric #-}
module Data.BitCode.LLVM.Codes.AtomicOrdering where

import GHC.Generics                      (Generic)
import Data.Binary                       (Binary)

-- | Encoded AtomicOrdering values.
data AtomicOrdering
  = ORDERING_NOTATOMIC -- 0
  | ORDERING_UNORDERED -- 1
  | ORDERING_MONOTONIC -- 2
  | ORDERING_ACQUIRE -- 3
  | ORDERING_RELEASE -- 4
  | ORDERING_ACQREL -- 5
  | ORDERING_SEQCST -- 6
  deriving (Show, Enum, Eq, Ord, Generic)

instance Binary AtomicOrdering
