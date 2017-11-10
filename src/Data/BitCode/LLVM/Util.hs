{-# OPTIONS_GHC -fprof-auto #-} 
module Data.BitCode.LLVM.Util where

import Text.PrettyPrint
import Data.BitCode.LLVM.Classes.HasType
import Data.BitCode.LLVM.Pretty

import qualified Data.BitCode.LLVM.Type as Ty
import qualified Data.BitCode.LLVM.Value as Val
import qualified Data.BitCode.LLVM.Instruction as Inst

import GHC.Stack

import Debug.Trace

isPtr :: HasCallStack => Ty.Ty -> Bool
isPtr (Ty.Ptr{}) = True
isPtr _ = False

isInt :: HasCallStack => Ty.Ty -> Bool
isInt (Ty.Int{}) = True
isInt _ = False

isArray :: HasCallStack => Ty.Ty -> Bool
isArray (Ty.Array{}) = True
isArray _ = False

isVector :: HasCallStack => Ty.Ty -> Bool
isVector (Ty.Vector{}) = True
isVector _ = False

isStruct :: HasCallStack => Ty.Ty -> Bool
isStruct (Ty.StructAnon{}) = True
isStruct (Ty.StructNamed{}) = True
isStruct _ = False

isFunction :: HasCallStack => Ty.Ty -> Bool
isFunction (Ty.Function{}) = True
isFunction _ = False

isFunctionPtr :: HasCallStack => Ty.Ty -> Bool
isFunctionPtr t = (isPtr t) && (isFunction (lower t))

-- | Checks if a type is of tyep @i1@.  LLVM requires
-- types to be i1 (bool) in binary conditonal position.
isBoolTy :: HasCallStack => Val.Symbol -> Bool
isBoolTy t | ty t == Ty.Int 1 = True
           | otherwise        = False

lift :: HasCallStack => Ty.Ty -> Ty.Ty
lift t = Ty.Ptr 0 t

lower :: HasCallStack => Ty.Ty -> Ty.Ty
lower (Ty.Ptr _ t) = t
lower t            = error . show $ text "Type:" <+> pretty t <+> text "cannot be lowerd."

elemTy :: HasCallStack => Ty.Ty -> Ty.Ty
elemTy (Ty.Array _ t)  = t
elemTy (Ty.Vector _ t) = t
elemTy t               = error . show $ text "Type:" <+> pretty t <+> text "does not have an element type."

elemTys :: HasCallStack => Ty.Ty -> [Ty.Ty]
elemTys (Ty.StructAnon _ ts)    = ts
elemTys (Ty.StructNamed _ _ ts) = ts
elemTys t                       = error . show $ text "Type:" <+> pretty t <+> text "does not have element types."

funRetTy :: HasCallStack => Ty.Ty -> Ty.Ty
funRetTy (Ty.Ptr _ (Ty.Function _ t _)) = t
funRetTy t                              = error . show $ text "Type:" <+> pretty t <+> parens (text (show t)) <+> text "is not a function type."

funParamTys :: HasCallStack => Ty.Ty -> [Ty.Ty]
funParamTys (Ty.Ptr _ (Ty.Function _ _ ts)) = ts
funParamTys t                               = error . show $ text "Type:" <+> pretty t <+> parens (text (show t)) <+> text "is not a function type."

-- TODO: Compute instruction type from the symbol!
-- TODO: make this an HasType for Instructions. Use error where invalid; and have a
--       separate function @hasResult@. (Or use undef?) As this is used in the tref
--       computation fold.
instTy :: HasCallStack => Inst.Inst -> Maybe Ty.Ty
instTy (Inst.Alloca t _ _)          = Just t
instTy (Inst.Cast t _ _)            = Just t
instTy (Inst.Load t _ _)            = Just t
instTy (Inst.Store{})               = Nothing
-- TODO: Maybe this should be at the callStie of instTy?
--       Instead of it being here.
instTy (Inst.Call t _ _ _ _ _ )     | t == Ty.Void = Nothing
                                    | otherwise    = Just t
instTy (Inst.Ret{})                 = Nothing
instTy (Inst.UBr{})                 = Nothing
instTy (Inst.Br{})                  = Nothing
instTy (Inst.Switch{})              = Nothing
instTy (Inst.Cmp2 t _ _ _)          = Just t
instTy (Inst.BinOp t _ _ _ _)       = Just t
instTy (Inst.CmpXchg p _ _ _ _ _)   = Just (Ty.StructAnon False [(lower (ty p)), Ty.Int 1]) -- { lower ty, i1 }
instTy (Inst.Fence{})               = Nothing
instTy (Inst.AtomicRMW p _ _ _ _)   = Just (lower (ty p))
instTy (Inst.AtomicStore{})         = Nothing
instTy (Inst.AtomicLoad t _ _ _ _)  = Just t

-- GEP returns a pointer to it's type.
instTy (Inst.Gep bt _ s idxs) | bt == ty s = Just $ lift $ drill (ty s) idxs
                              | otherwise  = error $ "Broken getElementPointer. Basetype: " ++ show bt ++ " and value type type: " ++ show (lower (ty s)) ++ " don't match!"
instTy (Inst.ExtractValue s idxs) = Just $ drill' (ty s) (map mkI32Val idxs)
  where mkI32Val = Val.Constant (Ty.Int 32) . Val.Int . fromIntegral

-- instTy i                       = error $ "No instTy for instruction: " ++ show i


-- TODO: Support Vector indexes.
drill :: HasCallStack => Ty.Ty -> [Val.Symbol] -> Ty.Ty
drill t = drill' t . map Val.symbolValue
drill' :: HasCallStack => Ty.Ty -> [Val.Value] -> Ty.Ty
drill' t [] = t
drill' (Ty.Array _ t)  (Val.Constant (Ty.Int{}) (Val.Int _):vs)  = drill' t vs
drill' t@(Ty.Array _ _)  (idx:_)                                 = error $ "Cannot drill into " ++ show (pretty t) ++ " with " ++ show (pretty idx)
drill' (Ty.Vector _ t) (Val.Constant (Ty.Int{}) (Val.Int _):vs)  = drill' t vs
drill' t@(Ty.Vector _ _) (idx:_)                            = error $ "Cannot drill into " ++ show t ++ " with " ++ show idx
drill' (Ty.Ptr _ t)    (Val.Constant (Ty.Int{}) (Val.Int _):vs)  = drill' t vs
drill' t@(Ty.Ptr _ _)    (idx:_)                            = error $ "Cannot drill into " ++ show t ++ " with " ++ show idx
drill' t@(Ty.StructAnon _ tys) (Val.Constant (Ty.Int 32) (Val.Int n):vs)
  | 0 <= n && n < length tys                              = drill' (tys !! n) vs
  | otherwise                                             = error $ "Cannot drill into struct " ++ show t ++ " with " ++ show n ++ "; index out of bounds!"
drill' t@(Ty.StructAnon _ _) (idx:_)                       = error $ "Can only drill into struct " ++ show t ++ " with int32, " ++ show idx ++ " given."
drill' t@(Ty.StructNamed _ _ tys) (Val.Constant (Ty.Int 32) (Val.Int n):vs)
  | 0 <= n && n < length tys                              = drill' (tys !! n) vs
  | otherwise                                             = error $ "Cannot drill into struct " ++ show t ++ " with " ++ show n ++ "; index out of bounds!"
drill' t@(Ty.StructNamed _ _ _) (idx:_)                    = error $ "Can only drill into struct " ++ show t ++ " with int32, " ++ show idx ++ " given."

-- The terminator instructions are: ‘ret‘, ‘br‘, ‘switch‘, ‘indirectbr‘, ‘invoke‘, ‘resume‘, ‘catchswitch‘, ‘catchret‘, ‘cleanupret‘, and ‘unreachable‘.
isTerminator :: HasCallStack => Inst.Inst -> Bool
isTerminator (Inst.Ret{})    = True
isTerminator (Inst.UBr{})    = True
isTerminator (Inst.Br{})     = True
isTerminator (Inst.Switch{}) = True
isTerminator _               = False
