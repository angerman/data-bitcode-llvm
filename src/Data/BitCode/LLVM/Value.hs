{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric   #-}

module Data.BitCode.LLVM.Value where

import Data.Word                         (Word16, Word32, Word64)
import Data.BitCode.LLVM.Type            (Ty)
import Data.BitCode.LLVM.Linkage         (Linkage)
import Data.BitCode.LLVM.Visibility      (Visibility)
import Data.BitCode.LLVM.ThreadLocalMode (ThreadLocalMode)
import Data.BitCode.LLVM.StorageClass    (DLLStorageClass)
import Data.BitCode.LLVM.CallingConv     (CallingConv)
import Data.BitCode.LLVM.Opcodes.Cast    (CastOp)
import Data.BitCode.LLVM.Opcodes.Binary  (BinOp)

import Data.BitCode.LLVM.Classes.HasType

import GHC.Generics                      (Generic)
import Data.Binary                       (Binary)

-- | Just a reference.
type Ref = Int

data FpValue
  = FpHalf         !Word16             -- ^ IEEEhalf
  | FpSingle       !Word32             -- ^ IEEEsingle
  | FpDouble       !Word64             -- ^ IEEEdouble
  | FpDoubleExt    !(Word64, Word64)  -- ^ X86 Double Extended (80)
  | FpQuad         !(Word64, Word64)  -- ^ IEEEquad
  | FpDoubleDouble !(Word64, Word64)  -- ^ PPCDoubleDouble
  deriving (Show, Eq, Ord, Generic)

-- | Const `types`. These are usually wrapped in a Constant.
-- which carries their type.
data Const
  = Null
  | Undef
  | Int !Int         -- ^ [intval]
  | WideInt ![Int]   -- ^ [n x intval]
  | Float !FpValue   -- ^ [fpval]
  -- aggregate constants. Undef is also an aggregate.
  | Array ![Symbol] -- ^ aggregate
  | Vector ![Symbol] -- ^ aggregate
  | Struct ![Symbol] -- ^ aggregate
  | String !String
  | CString !String
  | BinOp !BinOp !Symbol !Symbol -- ^ [opcode, opval, opval]
  | Cast !Ty !CastOp !Symbol     -- ^ [opcode, opty, opval]
  | Gep ![Word64]                -- ^ [n x operands]
  | Select !Word64 !Word64 !Word64 -- ^ [opval, opval, opval]
  | ExtractElt !Word64 !Word64 !Word64 -- ^ [opty, opval, opval]
  | InsertElt !Word64 !Word64 !Word64 -- ^ [opval, opval, opval]
  | ShuffleVec !Word64 !Word64 !Word64 -- ^ [opval, opval, opval]
  | Cmp !Word64 !Word64 !Word64 !Word64 -- ^ [opty, opval, opval, pred]
  -- | InlineAsm -- TODO
  | ShuffleVecEx !Word64 !Word64 !Word64 !Word64 -- ^ [opty, opval, opval, opval]
  | InboundsGep !Ty ![Symbol] -- ^ [[ty,] opty, opval,...]; if ty defaults to nullptr.
  | BlockAddress !Word64 !Word64 !Word64 -- ^ [fnty, fnval, bb#]
  | Data ![Word64] -- ^ [n x elements]
  -- | InlineAsm -- TODO
  deriving (Show, Eq, Ord, Generic)

-- | Values the ValueList may contain.
data Value
  -- | global variables
  = Global
    { gPointerType :: Ty              -- ^ The type index of the pointer type used to point to this global variable
    , gIsConst :: Bool                -- ^ Non-zero if the variable is treated as constant within the module, or zero if it is not
    , gAddressSpace :: Word64
    , gInit :: Maybe Symbol           -- ^ If non-zero, the value index of the initializer for this variable, plus 1.
    , gLinkage :: Linkage
    , gParamAttrs :: Word64
--    , gAlignment :: Word64          -- ^ The logarithm base 2 of the variable's requested alignment, plus 1
    -- TODO: turn this into a Maybe
    , gSection :: Word64              -- ^ If non-zero, the 1-based section index in the table of @MODULE_SECTION_NAME@.
    , gVisibility :: Visibility       -- ^ If present, an encoding of the visibility of this variable
    , gThreadLocal :: ThreadLocalMode -- ^ If present, an encoding of the thread local storage mode of the variable
    , gUnnamedAddr :: Bool            -- ^ If present and non-zero, indicates that the variable has @unnamed_addr@
    , gExternallyInitialized :: Bool
    , gDLLStorageClass :: DLLStorageClass -- ^ If present, an encoding of the DLL storage class of this variable
    , gComdat :: Word64 -- ???
    }
  -- | function values
  | Function
    { fType :: Ty                      -- ^ The type index of the function type describing this function
    , fCallingConv :: CallingConv
    , fIsProto :: Bool                 -- ^ Non-zero if this entry represents a declaration rather than a definition
    , fLinkage :: Linkage
    , fParamAttrs :: Word64            -- ^ If nonzero, the 1-based parameter attribute index into the table of @PARAMATTR_CODE_ENTRY@ entries.
    , fAlignment :: Word64
    , fSection :: Word64               -- ^ If non-zero, the 1-based section index in the table of @MODULE_CODE_SECTIONNAME@ entries.
    , fVisibility :: Visibility
    , fGC :: Word64                    -- ^ If present and nonzero, the 1-based garbage collector index in the table of @MODULE_CODE_GCNAME@ entries.
    , fUnnamedAddr :: Bool             -- ^ If present and non-zero, indicates that the function has @unnamed_addr@.
    , fPrologueData :: Maybe Symbol    -- ^ If non-zero, the value index of the prologue data for this function, plus 1.
    , fDLLStorageClass :: DLLStorageClass -- ^ An encoding of the DLL storage class of this function.
    , fComdat :: Word64                -- ^ An encoding of the COMDAT of this function
    , fPrefixData :: Maybe Symbol      -- ^ If non-zero, the value index of the prefix data for this function, plus 1.
    , fPersonalityFn :: Word64         -- ^ If non-zero, the value index of the personality function for this function, plus 1.
    }
  -- | The @ALIAS@ record (code 9) marks the definition of an alias.
  | Alias
    { aType :: Ty                      -- ^ The type index of the alias
    , aAddrSpace :: Word64
    , aVal  :: Symbol                  -- ^ The value index of the aliased value
    , aLinkage :: Linkage
    , aVisibility :: Visibility
    , aThreadLocal :: ThreadLocalMode  -- ^ If present, an encoding of the thread local storage mode of the variable
    , aUnnamedAddr :: Bool             -- ^ If present and non-zero, indicates that the function has @unnamed_addr@.
    , aDLLStorageClass :: DLLStorageClass
    }
  | Constant { cTy :: Ty, cConst :: Const } -- ^ constant values
  | Arg Ty Ref                         -- ^ function arguments, within function bodies
  | Value Ty                           -- ^ function values, from instructions
  -- | Typed reference
  | TRef Ty Ref                        -- ^ typed references are generated by instructions.
  -- Forward References; this is somewhat ugly :(
  | FwdRef Word64
  -- A label. Used during construction, if you want to refer to something by name,
  -- and only know the type you want it to be at.
  -- This will be pruned from the graph prior to writing it out.
  -- Makes only sense as a Named Value Symbol. Those that can be found (by name),
  -- will be replaced with their repsective global or function. Others will be
  -- replaced by external references.
  | Label Ty
  deriving (Show, Ord, Generic)

instance Eq Value where
  Global ty cst as i l pa s v tl ua ei sc c == Global ty' cst' as' i' l' pa' s' v' tl' ua' ei' sc' c'
    =   ty == ty'
    && cst == cst'
    &&  as == as'
    &&   i == i'
    &&   l == l'
    &&  pa == pa'
    &&   s == s'
    &&   v == v'
    &&  tl == tl'
    &&  ua == ua'
    &&  ei == ei'
    &&  sc == sc'
    &&   c == c'
  -- special case for Function; ignoring proto, prologue and prefix data.
  Function ty cc _ l pa a s v gc ua _ sc c _ pf == Function ty' cc' _ l' pa' a' s' v' gc' ua' _ sc' c' _ pf'
    =  ty == ty'
    && cc == cc'
    &&  l == l'
    && pa == pa'
    &&  a == a'
    &&  s == s'
    &&  v == v'
    && gc == gc'
    && ua == ua'
    && sc == sc'
    &&  c == c'
    && pf == pf'
  Alias ty as val l v tl ua sc == Alias ty' as' val' l' v' tl' ua' sc'
    =   ty == ty'
    &&  as == as'
    && val == val'
    &&   l == l'
    &&   v == v'
    &&  tl == tl'
    &&  ua == ua'
    &&  sc == sc'
  Constant ty c == Constant ty' c' = ty == ty' && c == c'
  Arg ty ref    == Arg ty' ref'    = ty == ty' && ref == ref'
  Value ty      == Value ty'       = ty == ty'
  TRef ty ref   == TRef ty' ref'   = ty == ty' && ref == ref'
  FwdRef n      == FwdRef n'       = n == n'
  -- This is wonky! Makes sense for named symbols only, where
  -- We have Named <name> (Label <ty>).
  Label t       == Label t'        = t == t'
  _             == _               = False

instance HasType Value where
  ty (Global{..}) = gPointerType
  ty (Function{..}) = fType
  ty (Alias{..}) = aType
  ty (Constant t _) = t
  ty (Arg t _) = t
  ty (Value t) = t
  ty (TRef t _)= t
  ty (Label t) = t

data Named a
  = Named !String a
  | Unnamed a
  deriving (Show, Eq, Ord, Generic)

instance Functor Named where
  fmap f (Named n x) = Named n (f x)
  fmap f (Unnamed x) = Unnamed (f x)

type Symbol = Named Value

symbolValue :: Symbol -> Value
symbolValue (Named _ v) = v
symbolValue (Unnamed v) = v

symbolName :: Symbol -> Maybe String
symbolName (Named s _) = Just s
symbolName (Unnamed _) = Nothing

type ValueSymbolTable = [(Int,ValueSymbolEntry)]

data ValueSymbolEntry
  = Entry !String -- ^ value id, string
  | FnEntry !Int !String  -- ^ value id, offset, string
  deriving Show

entryName :: ValueSymbolEntry -> String
entryName (Entry s) = s
entryName (FnEntry _ s) = s

instance Binary FpValue
instance Binary Const
instance Binary Value
instance Binary a => Binary (Named a)
