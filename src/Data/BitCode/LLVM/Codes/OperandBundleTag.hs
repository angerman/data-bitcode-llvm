module Data.BitCode.LLVM.Codes.OperandBundleTag where

data OperandBundleTag
  = OPERAND_BUNDLE_UNUSED0 -- 0
  -- | TAG: @[strchr x N]@
  | OPERAND_BUNDLE_TAG -- 1
  deriving (Show, Enum)
