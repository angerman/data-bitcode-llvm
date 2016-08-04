module Data.BitCode.LLVM.ThreadLocalMode where

-- see @include/llvm/IR/GlobalValue.h@
data ThreadLocalMode
  = NotThreadLocal -- 0
  | GeneralDynamicTLSModel
  | LocalDynamicTLSModel
  | InitialExecTLSModel
  | LocalExecTLSModel
  deriving (Eq, Enum, Show)
