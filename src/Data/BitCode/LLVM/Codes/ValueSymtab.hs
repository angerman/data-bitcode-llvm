module Data.BitCode.LLVM.Codes.ValueSymtab where

-- | Value symbol table codes.
data ValueSymtabCodes
  = VST_CODE_UNUSED0 -- 0
  -- | VST_ENTRY: @[valueid, namechar x N]@
  | VST_CODE_ENTRY -- 1
  -- | VST_BBENTRY: @[bbid, namechar x N]@
  | VST_CODE_BBENTRY -- 2
  -- | VST_FNENTRY: @[valueid, offset, namechar x N]@
  | VST_CODE_FNENTRY -- 3
  -- | VST_COMBINED_FNENTRY: @[offset, namechar x N]@
  | VST_CODE_COMBINED_FNENTRY --  4
  deriving (Show, Enum)
