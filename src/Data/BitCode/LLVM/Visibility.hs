module Data.BitCode.LLVM.Visibility where

-- see @include/llvm/IR/GlobalValue.h@
-- | An enumeration for the kinds of visibility of global values.
data Visibility
  -- | The GV is visible
  = Default -- 0
  -- | The GV is hidden
  | Hidden -- 1
  -- | The GV is protected
  | Protected -- 2
  deriving (Eq, Enum, Ord, Show)

