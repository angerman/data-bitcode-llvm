{-# LANGUAGE DeriveGeneric #-}
module Data.BitCode.LLVM.Linkage where

import GHC.Generics                      (Generic)
import Data.Binary                       (Binary)

-- see @include/llvm/IR/GlobalValue.h@
data Linkage
  -- | Externally visible function
  = External -- 0
  -- Obsolete
  | Obsolete_Implicit_Comdat -- 1
   -- | Special purpose, only applies to global arrays
  | Appending -- 2
  -- | Rename collisions when linking (static functions).
  | Internal -- 3
  -- Obsolete 
  | Obsolete_ImplicitComdat2 -- 4 
  -- Obsolete DLLImportLinkage
  | Obsolete_DLLImportLinkage -- 5
  -- Obsolete DLLExportLinkage
  | Obsolete_DLLExportLinkage -- 6
  -- | ExternalWeak linkage description.
  | ExternalWeak -- 7
  -- | Tentative definitions.
  | Common -- 8
  -- | Like Internal, but omit from symbol table.
  | Private -- 9
  -- Obsolete
  | Obsolete_Implicit_Comdat3 -- 10
  -- Obsolete
  | Obsolete_Implicit_Comdat4 -- 11
  -- | Available for inspection, not emission.
  | AvailableExternally -- 12
  -- Obsolete
  | Obsolete_LinkerPrivateLinkage -- 13
  -- Obsolete
  | Obsolete_LinkerPrivateWeakLinkage -- 14
  -- Obsolete
  | Obsolete_LinkOnceODRAutoHideLinkage -- 15
  -- | Keep one copy of named function when linking (weak)
  | WeakAny -- 16
  -- | Same, but only replaced by something equivalent.
  | WeakODR -- 17
  -- | Keep one copy of function when linking (inline)
  | LinkOnceAny -- 18
  -- | Same, but only replaced by something equivalent.
  | LinkOnceODR -- 19
  deriving (Enum, Eq, Ord, Show, Generic)

instance Binary Linkage
