module Data.BitCode.LLVM.Codes.TypeSymtab where

-- | The type symbol table only has one code (TST_ENTRY_CODE).
data TypeSymtab
  = TST_CODE_UNUSED0 -- 0
  -- | TST_ENTRY: @[typeid, namechar x N]@
  | TST_CODE_ENTRY -- 1
  deriving (Show, Enum)
