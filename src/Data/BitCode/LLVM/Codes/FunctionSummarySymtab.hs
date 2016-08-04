module Data.BitCode.LLVM.Codes.FunctionSummarySymtab where

-- | The function summary section uses different codes in the per-module
-- and combined index cases.
data FunctionSummarySymtab
  = FS_CODE_UNUSED0 -- 0
  -- | FS_ENTRY: @[valueid, islocal, instcount]@
  | FS_CODE_PERMODULE_ENTRY -- 1
  -- | FS_ENTRY: @[modid, instcount]@
  | FS_CODE_COMBINED_ENTRY -- 2
  deriving (Show, Enum)
