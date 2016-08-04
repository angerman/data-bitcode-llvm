module Data.BitCode.LLVM.Codes.Attribute where

-- | PARAMATTR blocks have code for defining a parameter attribute set.
data AttributeCode
  = PARAMATTR_UNUSED0 -- 0
  -- | ENTRY: [paramidx0, attr0, paramidx1, attr1...]. WARN: Will be removed in 4.0
  | PARAMATTR_CODE_ENTRY_OLD -- 1
  -- | ENTRY: [attrgrp0, attrgrp1, ...]
  | PARAMATTR_CODE_ENTRY -- 2
  -- | ENTRY: [id, idx, attr0, att1, ...]
  | PARAMATTR_GRP_CODE_ENTRY -- 3
  deriving (Show, Enum)
