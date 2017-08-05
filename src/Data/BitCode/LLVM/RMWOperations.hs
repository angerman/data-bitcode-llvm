{-# LANGUAGE DeriveGeneric #-}
module Data.BitCode.LLVM.RMWOperations where

import GHC.Generics                      (Generic)
import Data.Binary                       (Binary)

-- | These are values used in the bitcode files to encode AtomicRMW operations.
-- The values of these enums have no fixed relation to the LLVM IR enum
-- values.  Changing these will break compatibility with old files.
data RMWOperations
  = XCHG -- 0
  | ADD -- 1
  | SUB -- 2
  | AND -- 3
  | NAND -- 4
  | OR -- 5
  | XOR -- 6
  | MAX -- 7
  | MIN -- 8
  | UMAX -- 9
  | UMIN -- 10
  deriving (Show, Enum, Eq, Ord, Generic)

instance Binary RMWOperations

