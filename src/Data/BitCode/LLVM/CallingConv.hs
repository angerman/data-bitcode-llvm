module Data.BitCode.LLVM.CallingConv where

-- TODO: Add doc, see @include/llvm/IR/CallingConv.h@
data CallingConv
  = C            -- 0
  | CC_UNUSED1 | CC_UNUSED2 | CC_UNUSED3 | CC_UNUSED4 | CC_UNUSED5 | CC_UNUSED6 | CC_UNUSED7
  | Fast         -- 8
  | Cold         -- 9
  | GHC          -- 10
  | HiPE         -- 11
  | WebKit_JS    -- 12
  | AnyReg       -- 13
  | PreserveMost -- 14
  | PreserveAll  -- 15
  | Swift        -- 16
  | CxxFastTls   -- 17
  -- NOTE: There are more (64...)
  deriving (Eq, Enum, Show)
