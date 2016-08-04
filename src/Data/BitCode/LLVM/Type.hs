module Data.BitCode.LLVM.Type where

import Data.Word (Word64)

-- * Types
data Ty
  = NumEntry Word64
  | Void | Float | Double | Label | Opaque { oName :: String }
  | Int { teWidth :: Word64 }
  | Ptr { teAddressSpace :: Word64, tePointeeTy :: Ty }
--  | FnOld
  | Half
  | Array { teNumElts :: Word64, teEltTy :: Ty }
  | Vector { teNumElts :: Word64, teEltTy :: Ty }
  | X86Fp80
  | Fp128
  | Metadata
  | X86Mmx
  | StructAnon { teAnonIsPacked :: Bool, teAnonEltTy :: [Ty] }
  | StructNamed { teName :: String, teNamedIsPacked :: Bool, teNamedEltTy :: [Ty] }
  | Function { teVarArg :: Bool, teRetTy :: Ty, teParamTy :: [Ty] }
  | Token
  deriving (Show, Eq)

orderIdx :: Ty -> Int
orderIdx (NumEntry{}) = 0
orderIdx Void         = 1
orderIdx Float        = 2
orderIdx Double       = 3
orderIdx Label        = 4
orderIdx (Opaque{})   = 5
orderIdx (Int{})      = 6
orderIdx (Ptr{})      = 7
orderIdx Half         = 8
orderIdx (Array{})    = 9
orderIdx (Vector{})   = 10
orderIdx X86Fp80      = 11
orderIdx Fp128        = 12
orderIdx Metadata     = 13
orderIdx X86Mmx       = 14
orderIdx StructAnon{} = 15
orderIdx StructNamed{}= 16
orderIdx Function{}   = 17
orderIdx Token        = 18

isComplex :: Ty -> Bool
isComplex (Ptr{}) = True
isComplex (Array{}) = True
isComplex (Vector{}) = True
isComplex (StructAnon{}) = True
isComplex (StructNamed{}) = True
isComplex (Function{}) = True
isComplex _ = False

isPrimitive :: Ty -> Bool
isPrimitive = not . isComplex

instance Ord Ty where
  x <= y | isPrimitive x && isPrimitive y = orderIdx x <= orderIdx y
         | isPrimitive x && isComplex y = True
         | isComplex x && isComplex y = and $ (map (<=) (subTypes x)) <*> subTypes y
         | otherwise = False

subTypes :: Ty -> [Ty]
subTypes (Ptr _ t) = t:subTypes t
subTypes (Array _ t) = t:subTypes t
subTypes (Vector _ t) = t:subTypes t
subTypes (StructAnon _ ts) = concatMap (\t -> t:subTypes t) ts
subTypes (StructNamed _ _ ts) = concatMap (\t -> t:subTypes t) ts
subTypes (Function _ r pt) = concatMap (\t -> t:subTypes t) (r:pt)
subTypes _ = []
