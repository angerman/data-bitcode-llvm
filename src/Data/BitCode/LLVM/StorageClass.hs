{-# LANGUAGE DeriveGeneric #-}
module Data.BitCode.LLVM.StorageClass where

import GHC.Generics                      (Generic)
import Data.Binary                       (Binary)

-- see @include/llvm/IR/GlobalValue.h@
-- | Storage classes of global values for PE targets.
data DLLStorageClass
  = Default -- 0
  -- | Function to be imported from DLL
  | DLLImport -- 1
  -- | Function to be accessible from DLL.
  | DLLExport -- 2
  deriving (Eq, Enum, Ord, Show, Generic)

instance Binary DLLStorageClass
