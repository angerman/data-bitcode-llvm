{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.BitCode.LLVM.Classes.ToSymbols
  (ToSymbols(..)) where

import Data.BitCode.LLVM.Value       as V
import Data.BitCode.LLVM.Function    as F
import Data.BitCode.LLVM.Instruction as I
import Data.Maybe (catMaybes)

-- I guess this should just be Foldable or Traversable.

-- | Return the symbols of a given data type.
class ToSymbols a where
  symbols :: a -> [Symbol]
  fsymbols :: [Symbol] -> a -> [Symbol]

instance (ToSymbols a) => ToSymbols [a] where
  symbols = concatMap symbols
  fsymbols s_ = foldl fsymbols s_

instance ToSymbols Value where
  symbols V.Global{..}   | Just s <- gInit = symbols s
  symbols V.Function{..} = concatMap symbols $ catMaybes [fPrologueData, fPrefixData]
  symbols V.Alias{..}    = symbols aVal
  symbols V.Constant{..} = symbols cConst
  symbols _            = []

  fsymbols s_ V.Global{..}  | Just s <- gInit = fsymbols s_ s
  fsymbols s_ V.Function{..} = foldl fsymbols s_ (catMaybes [fPrologueData, fPrefixData])
  fsymbols s_ V.Alias{..}    = fsymbols s_ aVal
  fsymbols s_ V.Constant{..} = fsymbols s_ cConst
  fsymbols s_ _            = s_

instance ToSymbols Const where
  symbols (V.Array ss)         = concatMap symbols ss
  symbols (V.Vector ss)        = concatMap symbols ss
  symbols (V.Struct ss)        = concatMap symbols ss
  symbols (V.BinOp _ s s')     = symbols s ++ symbols s'
  symbols (V.Cast _ _ s)       = symbols s
  symbols (V.InboundsGep _ ss) = concatMap symbols ss
  symbols _                  = []
  fsymbols s_ (V.Array ss)     = foldl fsymbols s_ ss
  fsymbols s_ (V.Vector ss)    = foldl fsymbols s_ ss
  fsymbols s_ (V.Struct ss)    = foldl fsymbols s_ ss
  fsymbols s_ (V.BinOp _ s s') = foldl fsymbols s_ [s, s']
  fsymbols s_ (V.Cast _ _ s)   = foldl fsymbols s_ [s]
  fsymbols s_ (V.InboundsGep _ ss) = foldl fsymbols s_ ss
  fsymbols s_ _              = s_


instance ToSymbols BlockInst where
  symbols (Just s, i) = s:symbols i
  symbols (Nothing, i) = symbols i
  fsymbols s_ (Just s, i) = fsymbols (fsymbols s_ s) i
  fsymbols s_ (Nothing, i) = fsymbols s_ i

instance ToSymbols BasicBlock where
  symbols (BasicBlock insts) = concatMap symbols insts
  fsymbols s_ (BasicBlock insts) = foldl fsymbols s_ insts

instance ToSymbols Function where
  -- TODO: do we want to apply symbols on the result instead of only to const?
  symbols (F.Function sig const body) = symbols sig ++ concatMap symbols const ++ concatMap symbols body
  fsymbols s_ (F.Function sig const body) = foldl fsymbols (foldl fsymbols s_ (sig:const)) body

instance ToSymbols Symbol where
  symbols s = s:symbols (symbolValue s)
  fsymbols s_ s | s `elem` s_ = s_
                | otherwise   = fsymbols (s:s_) (symbolValue s)

instance ToSymbols Inst where
  symbols (I.Alloca _ s _)      = [s]
  symbols (I.Cast _ _ s)        = [s]
  symbols (I.Load _ s _)        = [s]
  symbols (I.Store s s' _)      = [s,s']
  symbols (I.Call _ _ _ s _ ss) = s:ss
  symbols (I.Cmp2 _ s s' _)     = [s,s']
  symbols (I.Gep _ _ s ss)      = s:ss
  symbols (I.ExtractValue _ s ss) = s:ss
  symbols (I.Ret (Just s))      = [s]
  symbols (I.Ret Nothing)       = []
  symbols (I.UBr _)             = []
  symbols (I.Br s _ _)          = [s]
  symbols (I.BinOp _ _ l r _)   = [l, r]
  symbols (I.Switch s _ sbs)    = s:map fst sbs
  symbols (I.CmpXchg s s' s'' _ _ _) = [s,s',s'']
  symbols (I.Fence _ _)         = []
  symbols (I.AtomicRMW s s' _ _ _) = [s,s']
  symbols (I.AtomicStore s s' _ _ _) = [s, s']
  symbols (I.AtomicLoad _ s _ _ _) = [s]

  fsymbols s_ (I.Alloca _ s _)      = fsymbols s_ s
  fsymbols s_ (I.Cast _ _ s)        = fsymbols s_ s
  fsymbols s_ (I.Load _ s _)        = fsymbols s_ s
  fsymbols s_ (I.Store s s' _)      = foldl fsymbols s_ [s,s']
  fsymbols s_ (I.Call _ _ _ s _ ss) = foldl fsymbols s_ (s:ss)
  fsymbols s_ (I.Cmp2 _ s s' _)     = foldl fsymbols s_ [s,s']
  fsymbols s_ (I.Gep _ _ s ss)      = foldl fsymbols s_ (s:ss)
  fsymbols s_ (I.ExtractValue _ s ss) = foldl fsymbols s_ (s:ss)
  fsymbols s_ (I.Ret (Just s))      = fsymbols s_ s
  fsymbols s_ (I.Ret Nothing)       = s_
  fsymbols s_ (I.UBr _)             = s_
  fsymbols s_ (I.Br s _ _)          = fsymbols s_ s
  fsymbols s_ (I.BinOp _ _ l r _)   = foldl fsymbols s_ [l, r]
  fsymbols s_ (I.Switch s _ sbs)    = foldl fsymbols s_ (s:map fst sbs)
  fsymbols s_ (I.CmpXchg p c n _ _ _) = foldl fsymbols s_ [p, c, n]
  fsymbols s_ (I.Fence _ _)         = []
  fsymbols s_ (I.AtomicRMW s s' _ _ _) = foldl fsymbols s_ [s, s']
  fsymbols s_ (I.AtomicStore s s' _ _ _) = foldl fsymbols s_ [s, s']
  fsymbols s_ (I.AtomicLoad _ s _ _ _) = fsymbols s_ s
