module Data.BitCode.LLVM.ParamAttr where

import Data.BitCode.LLVM.Codes.AttributeKind (AttributeKind)

import Data.Word (Word64)


data ParamAttrGroupIdx
  = Ret
  | Fun
  | Param Word64
  deriving (Show)

data ParamAttrEntry
  = Kind AttributeKind
  | Align Word64
  | StackAlign Word64
  | Pair String (Maybe String)
  deriving (Show)

data ParamAttrGroupEntry = GroupEntry { pgIdx :: ParamAttrGroupIdx
                                      , pgAttrs :: [ParamAttrEntry]
                                      } deriving Show

type Idx = Int
type GroupIdx = Idx
