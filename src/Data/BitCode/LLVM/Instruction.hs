module Data.BitCode.LLVM.Instruction where

import Data.BitCode.LLVM.Types
import Data.BitCode.LLVM.Classes.ToSymbols

import Data.BitCode.LLVM.Type  (Ty(Ptr))
import Data.BitCode.LLVM.Value (Symbol)
import Data.BitCode.LLVM.Cmp   (Predicate)

import Data.BitCode.LLVM.Opcodes.Binary (BinOp)
import Data.BitCode.LLVM.Opcodes.Cast (CastOp)
import Data.BitCode.LLVM.Flags (Flag)

data Inst
  -- | Ty and Value type should match up. If Ty is Metadata, then the Value is takes from the Metadata List
  -- else from the value list.
  = BinOp Ty BinOp Symbol Symbol [Flag]
  | Cast Ty CastOp Symbol
  | Alloca Ty Symbol Align  -- this would produce a typed ref. Ref, Alignment of 0 means, the backend can choose an appropriate alignment.
  -- | Load instruction
  | Load Ty Symbol Align
  -- | Store instruction. Store the Value in the Typed Ref.
  | Store Symbol Symbol Align
  -- | Call instruction. (Ty :: Ty, Fn :: Symbol, args :: [Symbol])
  | Call Ty Symbol [Symbol]
  -- | Compare
  | Cmp2 Ty Symbol Symbol Predicate
  -- | GEP
  | Gep
    Ty    -- ^ base type
    Bool  -- ^ inbounds
    Symbol -- ^ Value indexed into
    [Symbol] -- ^ indices.
  -- | Return Terminator
  | Ret (Maybe Symbol)
  -- | Unconditional branch
  | UBr BasicBlockId
  -- | Conditional branch
  | Br Symbol BasicBlockId BasicBlockId
  deriving (Show, Eq)

instance ToSymbols Inst where
  symbols (Alloca _ s _)    = [s]
  symbols (Cast _ _ s)      = [s]
  symbols (Load _ s _)      = [s]
  symbols (Store s s' _)    = [s,s']
  symbols (Call _ s ss)     = s:ss
  symbols (Cmp2 _ s s' _)   = [s,s']
  symbols (Gep _ _ s ss)    = s:ss
  symbols (Ret (Just s))    = [s]
  symbols (Ret Nothing)     = []
  symbols (UBr _)           = []
  symbols (Br s _ _)        = [s]
  symbols (BinOp _ _ l r _) = [l, r]


instTy :: Inst -> Maybe Ty
instTy (Alloca t _ _) = Just t
instTy (Cast t _ _) = Just t
instTy (Load t _ _) = Just t
instTy (Store{}) = Nothing
instTy (Call t _ _) = Just t
instTy (Ret{}) = Nothing
instTy (UBr{}) = Nothing
instTy (Br{}) = Nothing
instTy (Cmp2 t _ _ _) = Just t
instTy (BinOp t _ _ _ _) = Just t
-- GEP returns a pointer to it's type. In the
--     same address space.
-- TODO: This *is* incorrect.
--       the actual type woud have to be the type
--       of the index traversal into the value.
instTy (Gep t@(Ptr s _) _ _ _) = Just (Ptr s t)

isTerminator :: Inst -> Bool
isTerminator (Ret{}) = True
isTerminator (UBr{}) = True
isTerminator (Br{})  = True
isTerminator _       = False
