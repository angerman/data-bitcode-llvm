module Data.BitCode.LLVM.Codes.ModulePathSymtab where

-- | The module path symbol table only has one code (MST_CODE_ENTRY).
data ModulePathSymtab
  = MST_CODE_UNUSED0 -- 0
  -- | MST_ENTRY: @[modid, namechar x N]@
  | MST_CODE_ENTRY -- 1
  deriving (Show, Enum)
