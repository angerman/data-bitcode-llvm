module Data.BitCode.LLVM.Metadata where

import Data.Word (Word64)

import Data.BitCode.LLVM.Type (Ty)

data Metadata
  = MDString String
  | MDValue Ty Word64
  | MDNode [Metadata]
  | MDName String
  | MDDistinctNode [Metadata]
  -- | MDKind this is handled as (Int, String) map.
  | MDLocation { mdLocDistinct :: Bool, mdLocLine :: Word64, mdLocCol :: Word64, mdLocScope :: Word64, mdLocInlinedAt :: Word64 }
  | MDNamedNode String [Metadata] -- Nodes.
  -- TODO: Many more, see @Codes/Metadata.hs@
  deriving (Show)
