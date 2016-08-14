{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Data.BitCode.LLVM.ToBitCode where

import Data.BitCode (NBitCode, mkBlock, mkRec, mkEmptyRec, bitWidth)
import Data.BitCode.LLVM
import Data.BitCode.LLVM.Util
import Data.BitCode.LLVM.Function
import Data.BitCode.LLVM.Classes.HasType
import qualified Data.BitCode.LLVM.Value as V (Const(..), Value(..), Symbol(..), symbolValue)
import qualified Data.BitCode.LLVM.Type  as T (Ty(..), subTypes)
import qualified Data.BitCode.LLVM.Instruction as I (Inst(..), TailCallKind(..))
import Data.BitCode.LLVM.Flags

import Data.BitCode.LLVM.IDs.Blocks
import qualified Data.BitCode.LLVM.Codes.Identification as IC
import qualified Data.BitCode.LLVM.Codes.Module         as MC
import qualified Data.BitCode.LLVM.Codes.Type           as TC
import qualified Data.BitCode.LLVM.Codes.Constants      as CC
import qualified Data.BitCode.LLVM.Codes.Function       as FC
import qualified Data.BitCode.LLVM.Codes.ValueSymtab    as VST

import Data.BitCode.LLVM.Classes.ToSymbols
import Data.List (elemIndex, sort, sortOn, groupBy, nub)
import Data.Function (on)

import Data.Maybe (fromMaybe, catMaybes)
import Data.Word  (Word64)

import Data.Bits (FiniteBits, (.|.), shift, setBit)

import Debug.Trace

-- to compute the length of emitted bitcode.
import Data.BitCode (denormalize)
import Data.BitCode.Writer (emitTopLevel)
import Data.BitCode.Writer.Monad (evalBitCodeWriter, ask)

import Data.BitCode.LLVM.Pretty
import Data.BitCode.LLVM.Util
import Text.PrettyPrint ((<+>), text, (<>), int)
import Control.Applicative ((<|>))
--------------------------------------------------------------------------------
-- Turn things into NBitCode.
--
class ToNBitCode a where
  toBitCode :: a -> [NBitCode]

-- | Rerturns the Identification Block
instance ToNBitCode Ident where
  toBitCode (Ident name epoch)
    = pure $ mkBlock IDENTIFICATION [ mkRec IC.STRING name
                                    , mkRec IC.EPOCH epoch
                                    ]

instance (ToNBitCode a) => ToNBitCode (Maybe a) where
  toBitCode (Just a) = toBitCode a
  toBitCode Nothing  = []

instance (ToNBitCode a) => ToNBitCode [a] where
  toBitCode = concatMap toBitCode

instance {-# OVERLAPPING #-} ToNBitCode [T.Ty] where
  toBitCode tys
    = pure $ mkBlock TYPE_NEW (numEntryRec:concatMap mkTypeRec tys)
    where numEntryRec :: NBitCode
          numEntryRec = mkRec TC.NUMENTRY (length tys)
          mkTypeRec :: T.Ty -> [NBitCode]
          mkTypeRec T.Void                   = [ mkEmptyRec TC.VOID ]
          mkTypeRec T.Float                  = [ mkEmptyRec TC.FLOAT ]
          mkTypeRec T.Double                 = [ mkEmptyRec TC.DOUBLE ]
          mkTypeRec T.Label                  = [ mkEmptyRec TC.LABEL ]
          mkTypeRec (T.Opaque name)          = [ mkRec TC.STRUCT_NAME name, mkEmptyRec TC.OPAQUE ]
          mkTypeRec (T.Int w)                = [ mkRec TC.INTEGER [w] ]
          mkTypeRec (T.Ptr s t)              = [ mkRec TC.POINTER [lookupTypeIndex tys t, s] ]
          mkTypeRec T.Half                   = [ mkEmptyRec TC.HALF ]
          mkTypeRec (T.Array n t)            = [ mkRec TC.ARRAY [n, lookupTypeIndex tys t] ]
          mkTypeRec (T.Vector n t)           = [ mkRec TC.VECTOR [n, lookupTypeIndex tys t] ]
          mkTypeRec T.X86Fp80                = [ mkEmptyRec TC.X86_FP80 ]
          mkTypeRec T.Fp128                  = [ mkEmptyRec TC.FP128 ]
          mkTypeRec T.Metadata               = [ mkEmptyRec TC.METADATA ]
          mkTypeRec T.X86Mmx                 = [ mkEmptyRec TC.X86_MMX ]
          mkTypeRec (T.StructAnon p ts)      = [ mkRec TC.STRUCT_ANON ((if p then 1 else 0  :: Int):map (lookupTypeIndex tys) ts) ]
          mkTypeRec (T.StructNamed name p ts)= [ mkRec TC.STRUCT_NAME name, mkRec TC.STRUCT_NAMED ((if p then 1 else 0 :: Int):map (lookupTypeIndex tys) ts) ]
          mkTypeRec (T.Function vargs t ts)  = [ mkRec TC.FUNCTION ((if vargs then 1 else 0::Int):map (lookupTypeIndex tys) (t:ts)) ]
          mkTypeRec T.Token                  = [ mkEmptyRec TC.TOKEN ]




lookupIndexGeneric :: (Pretty a, Eq a, Show a, Integral b) => [a] -> a -> b
lookupIndexGeneric xs x = case elemIndex x xs of
  Just i -> fromIntegral i
  Nothing -> error . show $ text "Unable to find" <+> pretty x <+> text "in" <+> pretty xs

lookupTypeIndex :: Integral b => [T.Ty] -> T.Ty -> b
lookupTypeIndex ts t = case elemIndex t ts of
  Just i  -> fromIntegral i
  Nothing -> error . show $ text "Unable to find type" <+> pretty t <+> text "in" <+> pretty ts

lookupValueIndex :: Integral b => [V.Value] -> V.Value -> b
lookupValueIndex vs f@(V.Function{..}) = case (elemIndex f vs) <|> (elemIndex f { V.fIsProto = not fIsProto } vs) of
  Just i  -> fromIntegral i
  Nothing -> error . show $ text "Unable to find function" <+> pretty f <+> text "in" <+> pretty vs
lookupValueIndex vs v = case elemIndex v vs of
  Just i  -> fromIntegral i
  Nothing -> error . show $ text "Unable to find value" <+> pretty v <+> text "in" <+> pretty vs

lookupSymbolIndex :: Integral b => [V.Symbol] -> V.Symbol -> b
lookupSymbolIndex ss s = case elemIndex s ss of
  Just i  -> fromIntegral i
  Nothing -> error . show $ text "Unable to find symbol" <+> pretty s <+> text "in" <+> pretty ss

-- We *can not* have ToNBitCode Module, as we
-- need to know the position in the bitcode stream.
-- And this includes the Indetification :(
instance ToNBitCode (Maybe Ident, Module) where
  toBitCode (i, m@(Module{..}))
    = concat [ identBlock,
               pure . mkBlock MODULE $
                 moduleHeader ++
                 -- TODO: To support the OffsetLogic in 3.8 and later
                 --       we would replace the mkSynTabBlock with the VST Fwd Ref.
                 --       and put the mkSymTabBlock at the end (behind the mkFunctionBlock's.)
                 --       We would need to be able to figure out the bitcode positions though.
                 -- NOTE: An initial attempt at that (see nBitCodeLength below), is missing
                 --       some vital ingredent.  See also the CODE_FNENTRY generation.
                 [ mkSymTabBlock (constantSymbols ++ globalSymbols ++ functionSymbols) ] ++
                 map mkFunctionBlock mFns ++
                 []
      ]
    -- = pure $ mkBlock MODULE [ {- Record: Version 1 -}
    --                         , {- Block: ParamAttrGroup 10 -}
    --                         , {- Block: ParamAttr 9 -}
    --                         , {- Block: Types 17 -}
    --                         , {- Record: Triple 2 -}
    --                         , {- Record: Datalayout 3 -}
    --                         , {- Record: GlobalVar 7 -}, ...
    --                         , {- Record: Function 8 -}, ...
    --                         , {- Record: VSTOffset 13 -}
    --                         , {- Block: Constants 11 -}
    --                         , {- Block: Metadata 15 -}, ...
    --                         , {- Block: Function 12 -}
    --                         , {- Block: SymTab 14 -}
    --                         ]
    where
      --------------------------------------------------------------------------
      -- Compute the offsets
      identBlock   = toBitCode i
      moduleHeader = [ mkRec MC.VERSION [mVersion] ] ++
                     toBitCode allTypes ++
                     [ mkRec MC.TRIPLE t | Just t <- [mTriple] ] ++
                     [ mkRec MC.DATALAYOUT dl | Just dl <- [mDatalayout] ] ++
                     mkConstBlock constantSymbols constantSymbols ++
                     map mkGlobalRec (map V.symbolValue globalSymbols) ++
                     map mkFunctionRec (map V.symbolValue functionSymbols)
      nBitCodeLength :: [NBitCode] -> (Int,Int)
      nBitCodeLength nbc = evalBitCodeWriter $ (emitTopLevel . map denormalize $ nbc) >> ask
      --------------------------------------------------------------------------
      -- globals
      globalSymbols = [ g | g@(V.Named _ (V.Global{})) <- mValues] ++
                      [ g | g@(V.Unnamed (V.Global{})) <- mValues]
      -- functions
      functionSymbols = [ f | f@(V.Named _ (V.Function{})) <- mValues] ++
                        [ f | f@(V.Unnamed (V.Function{})) <- mValues]
      -- TODO: aliases??
      -- constants
      constantSymbols = sortOn (V.cTy . V.symbolValue) $ [ c | c@(V.Named _ (V.Constant{})) <- mValues] ++
                                                         [ c | c@(V.Unnamed (V.Constant{})) <- mValues]
      -- The types used in the module.
      topLevelTypes = nub . sort . map ty . symbols $ m
      -- all types contains all types including those that types reference. (e.g. i8** -> i8**, i8*, and i8)
      allTypes = nub . sort $ topLevelTypes ++ concatMap T.subTypes topLevelTypes


      -- | Turn a set of Constant Values unto BitCode Records.
      mkConstBlock :: [V.Symbol] -- ^ values that can be referenced.
                   -> [V.Symbol] -- ^ the constants to turn into BitCode
                   -> [NBitCode]
      mkConstBlock values consts | length consts > 0 = [ mkBlock CONSTANTS $ concatMap f (groupBy ((==) `on` (V.cTy . V.symbolValue)) consts) ]
                                 | otherwise = []
        where
          f :: [V.Symbol] -> [NBitCode]
          f [] = []
          f (s:cs) | (V.Constant t c) <- V.symbolValue s = (mkRec CC.CST_CODE_SETTYPE (lookupTypeIndex allTypes t :: Int)):mkConstRec values c:map (mkConstRec values . V.cConst . V.symbolValue) cs
                   | otherwise = error $ "Invalid constant " ++ show s
      mkConstRec :: [V.Symbol] -> V.Const -> NBitCode
      mkConstRec constantSymbols V.Null         = mkEmptyRec CC.CST_CODE_NULL
      mkConstRec constantSymbols V.Undef        = mkEmptyRec CC.CST_CODE_UNDEF
      mkConstRec constantSymbols (V.Int n)      = mkRec CC.CST_CODE_INTEGER (fromSigned n)
      mkConstRec constantSymbols (V.WideInt ns) = mkRec CC.CST_CODE_WIDE_INTEGER (map fromSigned ns)
      -- TODO: Float encoding?
--      mkConstRec constants (Float f) = mkRect CC.CST_CODE_FLOAT f
      -- TODO: Support aggregates (lookup value numbers in Constants? + Globals + Functions?)
--      mkConstRec constants (Aggregate valueNs)
      mkConstRec constantSymbols (V.String s) = mkRec CC.CST_CODE_STRING s
      mkConstRec constantSymbols (V.CString s) = mkRec CC.CST_CODE_CSTRING s
      -- XXX BinOp, Cast, Gep, Select, ExtractElt, InsertElt, ShuffleVec, Cmp, InlineAsm, ShuffleVecEx,
      mkConstRec constantSymbols (V.InboundsGep t symbls)
        = mkRec CC.CST_CODE_CE_INBOUNDS_GEP $
          (lookupTypeIndex allTypes t :: Int):zip' (map (lookupTypeIndex allTypes . ty) symbls)
                                               (map (lookupSymbolIndex constantSymbols) symbls)

      -- signedness encoding.
      -- see `toSigned` in FromBitCode.
      fromSigned :: (FiniteBits a, Ord a, Num a) => a -> a
      fromSigned v | v < 0 = 1 .|. shift (-v) 1
                   | otherwise = shift v 1
      -- XXX BlockAddress, Data, InlineAsm
      zip' :: [a] -> [a] -> [a]
      zip' [] [] = []
      zip' (h:t) (h':t') = h:h':zip' t t'

      bool :: (Integral a) => Bool -> a
      bool x = if x then 1 else 0

      fromEnum' :: (Enum a, Integral b) => a -> b
      fromEnum' = fromIntegral . fromEnum

      mkGlobalRec :: V.Value -> NBitCode
      mkGlobalRec (V.Global{..}) = mkRec MC.GLOBALVAR [ lookupTypeIndex allTypes t -- NOTE: We store the pointee type.
                                                      , 1 .|. shift (bool gIsConst) 1 .|. shift gAddressSpace 2
                                                      , fromMaybe 0 ((+1) . lookupSymbolIndex constantSymbols <$> gInit)
                                                      , fromEnum' gLinkage
                                                      , gParamAttrs
                                                      , gSection
                                                      , fromEnum' gVisibility
                                                      , fromEnum' gThreadLocal
                                                      , bool gUnnamedAddr
                                                      , bool gExternallyInitialized
                                                      , fromEnum' gDLLStorageClass
                                                      , gComdat
                                                      ]
                                   where (T.Ptr _ t) = gPointerType

      mkFunctionRec :: V.Value -> NBitCode
      mkFunctionRec (V.Function{..}) = mkRec MC.FUNCTION [ lookupTypeIndex allTypes t -- NOTE: Similar to Globals we store the pointee type.
                                                         , fromEnum' fCallingConv
                                                         , bool fIsProto
                                                         , fromEnum' fLinkage
                                                         , fParamAttrs
                                                         , fAlignment
                                                         , fSection
                                                         , fromEnum' fVisibility
                                                         , fGC
                                                         , bool fUnnamedAddr
                                                         , fPrologueData
                                                         , fromEnum' fDLLStorageClass
                                                         , fComdat
                                                         , fPrefixData
                                                         , fPersonalityFn
                                                         ]
                                       where (T.Ptr _ t) = fType

      --------------------------------------------------------------------------
      -- VALUE SYMBOL TABLE
      --
      mkSymTabBlock :: [V.Symbol] -> NBitCode
      -- TODO: drop `catMaybes`, once we support all symbols (FNENTRY, BBENTRY)
      mkSymTabBlock syms = mkBlock VALUE_SYMTAB (catMaybes (map mkSymTabRec namedIdxdSyms))
        where namedIdxdSyms = [(idx, name, value) | (idx, (V.Named name value)) <- zip [0..] syms]
              mkSymTabRec :: (Int, String, V.Value) -> Maybe NBitCode
              mkSymTabRec (n, nm, (V.Function{..})) | fIsProto  = Just (mkRec VST.VST_CODE_ENTRY (n:map fromEnum nm))
                                                    -- LLVM 3.8 comes with FNENTRY, which has offset at the
                                                    --          second position. This however requires computing the
                                                    --          offset corret.
                                                    -- XXX: VST OFFSETS
                                                    | otherwise = Just (mkRec VST.VST_CODE_ENTRY (n:map fromEnum nm))
                                                                  -- Just (mkRec VST.VST_CODE_FNENTRY (n:offset-1:map fromEnum nm))
                where offset = fst . nBitCodeLength $ [ mkBlock MODULE $ moduleHeader ]
              -- XXX: this is ok here, as anything else can just be named constants/globals.
              --      We simply can not encounter blocks just yet.
              mkSymTabRec (n, nm, _)                            = Just (mkRec VST.VST_CODE_ENTRY (n:map fromEnum nm))
      --------------------------------------------------------------------------
      -- FUNCTIONS (BasicBlocks)
      --
      mkFunctionBlock :: Function -> NBitCode
      {- Declare blocks, constants, instructions, vst -}
      mkFunctionBlock (Function sig consts bbs)
        = mkBlock FUNCTION $
          [ mkRec FC.DECLAREBLOCKS (length bbs) ] ++
          mkConstBlock bodyVals fconstants ++
          -- this is a *bit* ugly.
          -- We use a fold to carry the instruction count through
          -- the record creation. We also prepend records and hence
          -- have to reverse them in the end.
          -- TODO: Use better mappendable DataStructure.
          (reverse . snd $ foldl mkInstRecFold (0,[]) (concatMap blockInstructions bbs))
        where -- function arguments
              fArgTys = funParamTys (V.fType (V.symbolValue sig))
              fArgs = map (V.Unnamed . V.Arg) fArgTys
              -- function local constant
              fconstants :: [V.Symbol]
              fconstants = sortOn (V.cTy . V.symbolValue) (filter isConst consts)
              isConst :: V.Symbol -> Bool
              isConst c | (V.Constant{}) <- V.symbolValue c = True
                        | otherwise                         = False
              -- the values the body can reference.
              bodyVals :: [V.Symbol]
              -- constants, globals, functions (because we order them that way)
              -- plus fargs and fconstants per function body ontop of which are
              -- the references generated by the instructions will be placed.
              bodyVals = constantSymbols ++ globalSymbols ++ functionSymbols ++ fArgs ++ fconstants
              nBodyVals = length bodyVals

              blockInstructions :: BasicBlock -> [I.Inst]
              blockInstructions (BasicBlock insts) = map snd insts
              blockInstructions (NamedBlock _ insts) = map snd insts

              -- instruction values (e.g. values generated by instructions)
              instVals = map (V.Unnamed . (uncurry V.TRef)) $ zip [t | Just t <- map instTy (concatMap blockInstructions bbs)] [0..]

              -- all values. This will be used to lookup indices for values in.
              allVals = bodyVals ++ instVals

              -- These are in FromBitCode as well. TODO: Refactor move BitMasks into a common file.
              inAllocMask = shift (1 :: Int) 5
              explicitTypeMask = shift (1 :: Int) 6
              swiftErrorMask = shift (1 :: Int) 7

              -- Relative Symbol lookup
              lookupRelativeSymbolIndex :: (Integral a)
                                        => [V.Symbol] -- ^ values prior to entering the instruction block
                                        -> [V.Symbol] -- ^ instruction values
                                        -> Int       -- ^ current instruction count
                                        -> V.Symbol   -- ^ the symbol to lookup
                                        -> a
              lookupRelativeSymbolIndex vs ivs iN s = fromIntegral $ vN + iN - lookupSymbolIndex vals s
                where vN = length vs
                      vals = vs ++ ivs

              lookupRelativeSymbolIndex' :: (Integral a) => Int -> V.Symbol -> a
              lookupRelativeSymbolIndex' = lookupRelativeSymbolIndex bodyVals instVals

              -- Build instructions.
              mkInstRec :: Int -> I.Inst -> NBitCode
              mkInstRec n (I.BinOp _ op lhs rhs flags) = mkRec FC.INST_BINOP [ lookupRelativeSymbolIndex' n lhs
                                                                             , lookupRelativeSymbolIndex' n rhs
                                                                             , fromEnum' op
                                                                             , foldl setBit (0 :: Int) (map flagValue flags)
                                                                             ]
              -- TODO: There should be valueTypePair :: Int -> Symbol -> [_]
              --       which encodes a Symbol that has an index < n just
              --       as the symbol, and otherwise as valueNumber, TypeIndex.
              --       The `getValueType` function in the reader does precisely this
              --       in the inverse way.
              mkInstRec n (I.Cast t op s) = mkRec FC.INST_CAST [ traceShow (pretty s <> text "@" <> int (relSymbolIndex - n)) $ relSymbolIndex
--                                                               , lookupTypeIndex allTypes (ty s)
                                                               , lookupTypeIndex allTypes t
                                                               , fromEnum' op :: Int
                                                               ]
                where
                  relSymbolIndex = lookupRelativeSymbolIndex' n s
              -- TODO: If we want to support InAlloca, we need to extend Alloca. For now we will not set the flag.
              mkInstRec n (I.Alloca t s a) = mkRec FC.INST_ALLOCA [ lookupTypeIndex allTypes (lower t)
                                                                  , lookupTypeIndex allTypes . ty $ s
                                                                  , lookupSymbolIndex allVals s
                                                                  , explicitTypeMask .|. bitWidth a
                                                                  ]
              -- TODO: Support Volatile flag
              -- Verify that t is (lower (ty s)).
              mkInstRec n (I.Load _ s a) = mkRec FC.INST_LOAD [ lookupRelativeSymbolIndex' n s
                                                              , lookupTypeIndex allTypes . lower . ty $ s
                                                              , bitWidth a
                                                              , 0
                                                              ]
              -- TODO: Support Volatile flag
              mkInstRec n (I.Store ref val a) = mkRec FC.INST_STORE [ lookupRelativeSymbolIndex' n ref
                                                                    , lookupRelativeSymbolIndex' n val
                                                                    , bitWidth a
                                                                    , 0
                                                                    ]

              -- TODO: Support FMF and Explicit Type flags explicitly
              -- XXX: Call needs paramAttrs! -- Can use 0 for empty param set.
              mkInstRec n (I.Call _ tck cc s fnTy args) = mkRec FC.INST_CALL $ [ (0 :: Int) -- Fix PARAMATTR
                                                                               , cconv .|. explTy .|. tcKind
                                                                               -- FMF
                                                                               , lookupTypeIndex allTypes (lower fnTy)
                                                                               , lookupRelativeSymbolIndex' n s
                                                                               ] ++ map (lookupRelativeSymbolIndex' n) args
                                                          where cconv  = shift (fromEnum' cc) (fromEnum CALL_CCONV)
                                                                explTy = setBit 0 (fromEnum' CALL_EXPLICIT_TYPE)
                                                                tcKind = case tck of
                                                                           I.None     -> 0
                                                                           I.Tail     -> setBit 0 (fromEnum' CALL_TAIL)
                                                                           I.MustTail -> setBit 0 (fromEnum' CALL_MUSTTAIL)
                                                                           I.NoTail   -> setBit 0 (fromEnum' CALL_NOTAIL)

              mkInstRec n (I.Cmp2 _ lhs rhs pred) = mkRec FC.INST_CMP2 [ lookupRelativeSymbolIndex' n lhs
                                                                                 , lookupRelativeSymbolIndex' n rhs
                                                                                 , fromEnum' pred :: Int
                                                                                 ]

              mkInstRec n (I.Gep ty inbounds base idxs) = mkRec FC.INST_GEP $ [ (bool inbounds) :: Int
                                                                              , lookupTypeIndex allTypes (lower ty)]
                                                          ++ map (lookupRelativeSymbolIndex' n) (base:idxs)

              mkInstRec n (I.Ret (Just val)) = mkRec FC.INST_RET [ lookupRelativeSymbolIndex' n val :: Int ]
              mkInstRec n (I.Ret Nothing)    = mkEmptyRec FC.INST_RET
              mkInstRec n (I.UBr bbId)       = mkRec FC.INST_BR [bbId]
              mkInstRec n (I.Br val bbId bbId') = mkRec FC.INST_BR [ bbId
                                                                   , bbId'
                                                                   , lookupRelativeSymbolIndex' n val
                                                                   ]
              mkInstRec n i = error $ "Instruction " ++ (show i) ++ " not yet supported."
              -- Fold helper to keep track of the instruction count.
              mkInstRecFold :: (Int, [NBitCode]) -> I.Inst -> (Int, [NBitCode])
              mkInstRecFold (n, codes) inst = case instTy inst of
                Just _ -> (n+1,mkInstRec n inst:codes)
                Nothing -> (n, mkInstRec n inst:codes)
