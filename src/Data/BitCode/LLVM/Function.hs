{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.BitCode.LLVM.Function where

import Data.BitCode.LLVM.Types
import Data.BitCode.LLVM.Classes.ToSymbols

import Data.BitCode.LLVM.Value       (Symbol, Value)
import Data.BitCode.LLVM.Instruction (Inst)

-- | Function declarations are set of so called basic blocks,
-- which contain sets of instructions.  These blocks may have
-- labels.
type BlockInst = (Maybe Symbol, Inst)

instance ToSymbols BlockInst where
  symbols (Just s, i) = s:symbols i
  symbols (Nothing, i) = symbols i

data BasicBlock
  = BasicBlock [BlockInst]
  | NamedBlock Label [BlockInst]
  deriving (Show, Eq)

instance ToSymbols BasicBlock where
  symbols (BasicBlock insts) = concatMap symbols insts

-- | Function definitions.
-- TODO: dSig is somewhat ugly, I'd lke enforce only function values here.
data Function = Function { dSig :: Symbol, dConst :: [Symbol], dBody :: [BasicBlock] }
  deriving (Show, Eq)

instance ToSymbols Function where
  symbols (Function sig const body) = sig:const ++ concatMap symbols body
