{-# LANGUAGE DeriveGeneric #-}
module Data.BitCode.LLVM.Linkage where

import GHC.Generics                      (Generic)
import Data.Binary                       (Binary)

-- see @include/llvm/IR/GlobalValue.h@
data Linkage
  -- | Externally visible function
  = External -- 0
  -- | Available for inspection, not emission.
  | AvailableExternally -- 1
  -- | Keep one copy of function when linking (inline)
  | LinkOnceAny -- 2
  -- | Same, but only replaced by something equivalent.
  | LinkOnceODR -- 3
  -- | Keep one copy of named function when linking (weak)
  | WeakAny -- 4
  -- | Same, but only replaced by something equivalent.
  | WeakODR -- 5
  -- | Special purpose, only applies to global arrays
  | Appending -- 6
  -- | Rename collisions when linking (static functions).
  | Internal -- 7
  -- | Like Internal, but omit from symbol table.
  | Private -- 8
  -- | ExternalWeak linkage description.
  | ExternalWeak -- 9
  -- | Tentative definitions.
  | Common -- 10
  deriving (Enum, Eq, Ord, Show, Generic)

instance Binary Linkage
