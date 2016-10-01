{-# LANGUAGE DeriveGeneric #-}

module Data.BitCode.LLVM.ThreadLocalMode where

import GHC.Generics                      (Generic)
import Data.Binary                       (Binary)

-- see @include/llvm/IR/GlobalValue.h@
data ThreadLocalMode
  = NotThreadLocal -- 0
  | GeneralDynamicTLSModel
  | LocalDynamicTLSModel
  | InitialExecTLSModel
  | LocalExecTLSModel
  deriving (Eq, Enum, Ord, Show, Generic)

instance Binary ThreadLocalMode
