{-# LANGUAGE DeriveGeneric #-}
module Data.BitCode.LLVM.Codes.SynchronizationScope where

import GHC.Generics                      (Generic)
import Data.Binary                       (Binary)

-- | Encoded SynchronizationScope values.
-- LLVM at some point gaind the ability to
-- encode custom sync scope ids. Those are
-- aggreagted in the sync scope ids, and
-- start at offset 2.
data AtomicSynchScope
  = SINGLE_THREAD -- 0
  | CROSS_THREAD -- 1  -- also known as SyncScope::System
  deriving (Show, Enum, Eq, Ord, Generic)

instance Binary AtomicSynchScope
