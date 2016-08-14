module Data.BitCode.LLVM.Classes.HasType where

import Data.BitCode.LLVM.Type

class HasType a where
  ty :: a -> Ty
