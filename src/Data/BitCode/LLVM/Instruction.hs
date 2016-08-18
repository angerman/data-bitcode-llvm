module Data.BitCode.LLVM.Instruction where

import Data.BitCode.LLVM.Types

import Data.BitCode.LLVM.Type  (Ty)
import Data.BitCode.LLVM.Value (Symbol)
import Data.BitCode.LLVM.Cmp   (Predicate)

import Data.BitCode.LLVM.Opcodes.Binary (BinOp)
import Data.BitCode.LLVM.Opcodes.Cast (CastOp)
import Data.BitCode.LLVM.CallingConv     (CallingConv)
import Data.BitCode.LLVM.Flags (Flag)

data TailCallKind = None | Tail | MustTail | NoTail deriving (Eq, Show)

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
  | Call { cTy  :: Ty           -- ^ result type
         , cTCK :: TailCallKind -- ^ tail call; should default to None
         , cCC  :: CallingConv  -- ^ the calling convention
         , cSym :: Symbol       -- ^ function or reference
                 -- TODO: if we make the function signature optional (Maybe Ty), could that produce an implicity type?
         , cSig :: Ty           -- ^ function signature     -- e.g. this should be equivalent to @lower . ty@
         , cArgs:: [Symbol]     -- ^ arguments
         }
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
  -- | Switch
  | Switch Symbol BasicBlockId [(Symbol, BasicBlockId)]
  deriving (Show, Eq)


