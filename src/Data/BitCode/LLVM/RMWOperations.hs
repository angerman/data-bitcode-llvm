{-# LANGUAGE DeriveGeneric #-}
module Data.BitCode.LLVM.RMWOperations where

import GHC.Generics                      (Generic)
import Data.Binary                       (Binary)

-- | These are values used in the bitcode files to encode AtomicRMW operations.
-- The values of these enums have no fixed relation to the LLVM IR enum
-- values.  Changing these will break compatibility with old files.
data RMWOperations
  = RWM_XCHG -- 0
  | RWM_ADD -- 1
  | RMW_SUB -- 2
  | RMW_AND -- 3
  | RMW_NAND -- 4
  | RMW_OR -- 5
  | RMW_XOR -- 6
  | RMW_MAX -- 7
  | RMW_MIN -- 8
  | RMW_UMAX -- 9
  | RMW_UMIN -- 10
  deriving (Show, Enum, Eq, Ord, Generic)

instance Binary RMWOperations

