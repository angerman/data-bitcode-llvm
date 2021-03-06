{-# LANGUAGE DeriveGeneric #-}
module Data.BitCode.LLVM.Codes.Identification where

import GHC.Generics                      (Generic)
import Data.Binary                       (Binary)

-- | Identification block contains a string that describes the producer details,
-- and an epoch that defines the auto-upgrade capability.
data IdentificationCodes
  -- There is no 0.
  = UNUSED0 -- 0
  -- | IDENTIFICATION: @[strchr x N]@
  | STRING -- 1
  -- | EPOCH: @[epoch#]@ -- see @Epoch@
  | EPOCH -- 2
  deriving (Show, Enum, Generic)

instance Binary IdentificationCodes

-- | The epoch that defines the auto-upgrade compatibility for the bitcode.
--
-- LLVM guarantees in a major release that a minor release can read bitcode
-- generated by previous minor releases. We translate this by making the reader
-- accepting only bitcode with the same epoch, except for the X.0 release which
-- also accepts N-1.
data Epoch
  -- | The current bitcode epoch
  = Current -- 0
  deriving (Show, Enum, Generic)

instance Binary Epoch
