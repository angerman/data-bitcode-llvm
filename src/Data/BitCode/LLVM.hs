{-# LANGUAGE RecordWildCards #-}
module Data.BitCode.LLVM where

import Data.BitCode.LLVM.ParamAttr (GroupIdx, ParamAttrGroupEntry)
import Data.BitCode.LLVM.Type  (Ty)
import Data.BitCode.LLVM.Value (ValueSymbolTable)
import Data.BitCode.LLVM.Function        (Function)
import Data.BitCode.LLVM.Value           (Symbol, Value, symbolValue)
import Data.BitCode.LLVM.Codes.Identification (Epoch)

import qualified Data.BitCode.LLVM.Instruction as I
import qualified Data.BitCode.LLVM.Value as V

import Data.BitCode.LLVM.Classes.ToSymbols

import Data.Word (Word64)

--- LLVM Bit Codes -------------------------------------------------------------
-- see LLVMBitCodes.h (e.g. http://llvm.org/docs/doxygen/html/LLVMBitCodes_8h_source.html)
--
-- Emacs Query Replace:
--  regexp: ^[[:digit:]]+[[:space:]]+\([[:alpha:]_]+\)[[:space:]]*=[[:space:]]*\([[:digit:]]+\)[,[:space:]]*//[[:space:]]*\(.+\)
--  with: -- | \3^J  | \1 -- \2
--  ^J: C-q C-j
--
--  if you did not copy off the website, you won't need the [[:digit:]]+ in front.
--  some fixing by hand is still required.  As some comments span multiple lines.
--

-- In general, the BlockIDs can be found in LLVM.IDs
-- the coresponding Record Codes in LLVM.Codes.XYZ


-- | The Identifier for the writer of this bitcode file
-- to aid in error reporting.
data Ident = Ident
             String -- ^ The name of the producer.
             Epoch  -- ^ The llvm bitcode epoch (version). Currently only 1 (Current) is supported.
  deriving Show

-- | The representation of the actual module.
data Module = Module
  { mVersion :: Word64         -- ^ Encoding version: 0 absolute indices, 1 relative indices.
  , mTriple :: Maybe String    -- ^ Optional triple: usually <arch>-<vendor>-<os>
  , mDatalayout :: Maybe String-- ^ Optional data layout string.
  , mValues :: [Symbol]        -- ^ Globals. (Global values and constants.)
  , mDecls :: [Symbol]         -- ^ Function declarations for functions outside of the module.
  , mFns :: [Function]         -- ^ Function definitions for function contained within the module.
  }
  deriving (Show,Eq)

instance ToSymbols Module where
  symbols (Module{..}) = mValues ++ mDecls ++ concatMap symbols mFns


class HasType a where
  ty :: a -> Ty

instance HasType Value where
  ty (V.Global{..}) = gPointerType
  ty (V.Function{..}) = fType
  ty (V.Alias{..}) = aType
  ty (V.Constant t _) = t
  ty (V.Arg t) = t
  ty (V.Value t) = t
  ty (V.TRef t _)= t

instance HasType Symbol where
  ty = ty . symbolValue

-- TODO: when actually constructing a module, we might
-- want a different data structure, which implicitly
-- creates the type table, and replaces types with
-- their respective indices.
