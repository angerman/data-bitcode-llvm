{-# LANGUAGE RecordWildCards #-}
module Data.BitCode.LLVM.Value where

import Data.Word                         (Word64)
import Data.BitCode.LLVM.Type            (Ty)
import Data.BitCode.LLVM.Linkage         (Linkage)
import Data.BitCode.LLVM.Visibility      (Visibility)
import Data.BitCode.LLVM.ThreadLocalMode (ThreadLocalMode)
import Data.BitCode.LLVM.StorageClass    (DLLStorageClass)
import Data.BitCode.LLVM.CallingConv     (CallingConv)
import Data.BitCode.LLVM.Opcodes.Cast    (CastOp)
import Data.BitCode.LLVM.Opcodes.Binary  (BinOp)

-- | Just a reference.
type Ref = Int

-- | Const `types`. These are usually wrapped in a Constant.
-- which carries their type.
data Const
  = Null
  | Undef
  | Int !Int         -- ^ [intval]
  | WideInt ![Int]   -- ^ [n x intval]
  | Float !Float   -- ^ [fpval]
  -- aggregate constants. Undef is also an aggregate.
  | Array ![Symbol] -- ^ aggregate
  | Vector ![Symbol] -- ^ aggregate
  | Struct ![Symbol] -- ^ aggregate
  | String !String
  | CString !String
  | BinOp !BinOp !Symbol !Symbol -- ^ [opcode, opval, opval]
  | Cast !CastOp !Ty !Symbol     -- ^ [opcode, opty, opval]
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
  deriving (Show, Eq)

-- | Values the ValueList may contain.
data Value
  -- | global variables
  = Global
    { gPointerType :: Ty              -- ^ The type index of the pointer type used to point to this global variable
    , gIsConst :: Bool                -- ^ Non-zero if the variable is treated as constant within the module, or zero if it is not
    , gAddressSpace :: Word64
    , gInit :: Maybe Value            -- ^ If non-zero, the value index of the initializer for this variable, plus 1.
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
    , fPrologueData :: Word64          -- ^ If non-zero, the value index of the prologue data for this function, plus 1.
    , fDLLStorageClass :: DLLStorageClass -- ^ An encoding of the DLL storage class of this function.
    , fComdat :: Word64                -- ^ An encoding of the COMDAT of this function
    , fPrefixData :: Word64            -- ^ If non-zero, the value index of the prefix data for this function, plus 1.
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
  | Constant { cTy :: Ty, cConst :: Const} -- ^ constant values
  | Arg Ty                             -- ^ function arguments, within function bodies
  | Value Ty                           -- ^ function values, from instructions
  -- | Typed reference
  | TRef Ty Ref                        -- ^ typed references are generated by instructions.
  -- Forward References; this is somewhat ugly :(
  | FwdRef Word64
  deriving (Show, Eq)

data Symbol
  = Named !String Value
  | Unnamed Value
  deriving (Show, Eq)

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
