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
orderIdx Half         = 7
orderIdx (Array{})    = 8
orderIdx (Vector{})   = 9
orderIdx X86Fp80      = 10
orderIdx Fp128        = 11
orderIdx Metadata     = 12
orderIdx X86Mmx       = 13
orderIdx StructAnon{} = 14
orderIdx StructNamed{}= 15
orderIdx Function{}   = 16
orderIdx Token        = 17
orderIdx (Ptr{})      = 18

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

isPtr :: Ty -> Bool
isPtr (Ptr{}) = True
isPtr _       = False

instance Ord Ty where
  x <= y | isPrimitive x && isPrimitive y = orderIdx x <= orderIdx y
         -- primitives first
         | isPrimitive x && isComplex y = True
         | isComplex x && isComplex y = x `elem` (subTypes y) || and ((map (<=) (subTypes x)) <*> subTypes y)
         | otherwise = False

subTypes :: Ty -> [Ty]
subTypes (Ptr _ t) = t:subTypes t
subTypes (Array _ t) = t:subTypes t
subTypes (Vector _ t) = t:subTypes t
subTypes (StructAnon _ ts) = concatMap (\t -> t:subTypes t) ts
subTypes (StructNamed _ _ ts) = concatMap (\t -> t:subTypes t) ts
subTypes (Function _ r pt) = concatMap (\t -> t:subTypes t) (r:pt)
subTypes _ = []

ftypes :: [Ty] -> Ty -> [Ty]
ftypes tys t | t `elem` tys = tys
ftypes tys t = case t of
  (Ptr _ t')                -> ftypes tys t' ++ [t]
  (Array _ t')              -> ftypes tys t' ++ [t]
  (Vector _ t')             -> ftypes tys t' ++ [t]
  (StructAnon _ ts)         -> foldl ftypes tys ts ++ [t]
  (StructNamed _ _ ts)      -> foldl ftypes tys ts ++ [t]
  (Function _ r pt)         -> foldl ftypes tys (r:pt) ++ [t]
  _                         -> t:tys
