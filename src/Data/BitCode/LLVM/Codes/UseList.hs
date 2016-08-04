module Data.BitCode.LLVM.Codes.UseList where

data UseList
  = USELIST_UNUSED0
  -- | DEFAULT: [index..., value-id]
  | USELIST_CODE_DEFAULT -- 1
  -- | BB: [index..., bb-id]
  | USELIST_CODE_BB -- 2
  deriving (Show, Enum)
