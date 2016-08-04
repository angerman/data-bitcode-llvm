module Data.BitCode.LLVM.Classes.ToSymbols where
import Data.BitCode.LLVM.Value (Symbol)

-- | Return the symbols of a given data type.
class ToSymbols a where
  symbols :: a -> [Symbol]
