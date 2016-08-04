module Data.BitCode.LLVM.Codes.SynchronizationScope where

-- | Encoded SynchronizationScope values.
data AtomicSynchScope
  = SYNCHSCOPE_SINGLETHREAD -- 0
  | SYNCHSCOPE_CROSSTHREAD -- 1
  deriving (Show, Enum)
