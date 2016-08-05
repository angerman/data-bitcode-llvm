{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Data.BitCode.LLVM.ToBitCode where

import Data.BitCode (NBitCode, mkBlock, mkRec, mkEmptyRec, bitWidth)
import Data.BitCode.LLVM
import Data.BitCode.LLVM.Function
import qualified Data.BitCode.LLVM.Value as V (Const(..), Value(..), Symbol(..), symbolValue)
import qualified Data.BitCode.LLVM.Type  as T (Ty(..), subTypes)
import qualified Data.BitCode.LLVM.Instruction as I (Inst(..), instTy)
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
          mkTypeRec (T.Ptr s t)              = [ mkRec TC.POINTER [lookupIndex tys t, s] ]
          mkTypeRec T.Half                   = [ mkEmptyRec TC.HALF ]
          mkTypeRec (T.Array n t)            = [ mkRec TC.ARRAY [n, lookupIndex tys t] ]
          mkTypeRec (T.Vector n t)           = [ mkRec TC.VECTOR [n, lookupIndex tys t] ]
          mkTypeRec T.X86Fp80                = [ mkEmptyRec TC.X86_FP80 ]
          mkTypeRec T.Fp128                  = [ mkEmptyRec TC.FP128 ]
          mkTypeRec T.Metadata               = [ mkEmptyRec TC.METADATA ]
          mkTypeRec T.X86Mmx                 = [ mkEmptyRec TC.X86_MMX ]
          mkTypeRec (T.StructAnon p ts)      = [ mkRec TC.STRUCT_ANON ((if p then 1 else 0  :: Int):map (lookupIndex tys) ts) ]
          mkTypeRec (T.StructNamed name p ts)= [ mkRec TC.STRUCT_NAME name, mkRec TC.STRUCT_NAMED ((if p then 1 else 0 :: Int):map (lookupIndex tys) ts) ]
          mkTypeRec (T.Function vargs t ts)  = [ mkRec TC.FUNCTION ((if vargs then 1 else 0::Int):map (lookupIndex tys) (t:ts)) ]
          mkTypeRec T.Token                  = [ mkEmptyRec TC.TOKEN ]

lookupIndex :: (Pretty a, Eq a, Show a, Integral b) => [a] -> a -> b
lookupIndex xs x = case elemIndex x xs of
  Just i -> fromIntegral i
  Nothing -> error $ "Unable to find " ++ (show $ pretty x) ++ " in " ++ (show $ map pretty xs)


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
                     mkConstBlock constants constants ++
                     map mkGlobalRec globals ++
                     map mkFunctionRec functions
      nBitCodeLength :: [NBitCode] -> (Int,Int)
      nBitCodeLength nbc = evalBitCodeWriter $ (emitTopLevel . map denormalize $ nbc) >> ask
      --------------------------------------------------------------------------
      -- globals
      globalSymbols = [ g | g@(V.Named _ (V.Global{})) <- mValues] ++
                      [ g | g@(V.Unnamed (V.Global{})) <- mValues]
      globals = map V.symbolValue globalSymbols
      -- functions
      functionSymbols = [ f | f@(V.Named _ (V.Function{})) <- mValues] ++
                        [ f | f@(V.Unnamed (V.Function{})) <- mValues]
      functions = map V.symbolValue functionSymbols
      -- TODO: aliases??
      -- constants
      constantSymbols = sortOn (V.cTy . V.symbolValue) $ [ c | c@(V.Named _ (V.Constant{})) <- mValues] ++
                                                         [ c | c@(V.Unnamed (V.Constant{})) <- mValues]
      constants = map V.symbolValue constantSymbols
      -- The types used in the module.
      topLevelTypes = nub . sort . map ty . symbols $ m
      -- all types contains all types including those that types reference. (e.g. i8** -> i8**, i8*, and i8)
      allTypes = nub . sort $ topLevelTypes ++ concatMap T.subTypes topLevelTypes


      -- | Turn a set of Constant Values unto BitCode Records.
      mkConstBlock :: [V.Value] -- ^ values that can be referenced.
                   -> [V.Value] -- ^ the constants to turn into BitCode
                   -> [NBitCode]
      mkConstBlock values consts | length consts > 0 = [ mkBlock CONSTANTS $ concatMap f (groupBy ((==) `on` V.cTy) consts) ]
                                 | otherwise = []
        where f [] = []
              f ((V.Constant t c):cs) = (mkRec CC.CST_CODE_SETTYPE (lookupIndex allTypes t :: Int)):mkConstRec values c:map (mkConstRec values . V.cConst) cs
      mkConstRec :: [V.Value] -> V.Const -> NBitCode
      mkConstRec constants V.Null         = mkEmptyRec CC.CST_CODE_NULL
      mkConstRec constants V.Undef        = mkEmptyRec CC.CST_CODE_UNDEF
      mkConstRec constants (V.Int n)      = mkRec CC.CST_CODE_INTEGER (fromSigned n)
      mkConstRec constants (V.WideInt ns) = mkRec CC.CST_CODE_WIDE_INTEGER (map fromSigned ns)
      -- TODO: Float encoding?
--      mkConstRec constants (Float f) = mkRect CC.CST_CODE_FLOAT f
      -- TODO: Support aggregates (lookup value numbers in Constants? + Globals + Functions?)
--      mkConstRec constants (Aggregate valueNs)
      mkConstRec constants (V.String s) = mkRec CC.CST_CODE_STRING s
      mkConstRec constants (V.CString s) = mkRec CC.CST_CODE_CSTRING s
      -- XXX BinOp, Cast, Gep, Select, ExtractElt, InsertElt, ShuffleVec, Cmp, InlineAsm, ShuffleVecEx,
      mkConstRec constants (V.InboundsGep t symbls)
        = mkRec CC.CST_CODE_CE_INBOUNDS_GEP $
          (lookupIndex allTypes t :: Int):zip' (map (lookupIndex allTypes . ty . V.symbolValue) symbls)
                                               (map (lookupIndex constants . V.symbolValue) symbls)

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
      mkGlobalRec (V.Global{..}) = mkRec MC.GLOBALVAR [ lookupIndex allTypes t -- NOTE: We store the pointee type.
                                                      , 1 .|. shift (bool gIsConst) 1 .|. shift gAddressSpace 2
                                                      , fromMaybe 0 ((+1) . lookupIndex constants <$> gInit)
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
      mkFunctionRec (V.Function{..}) = mkRec MC.FUNCTION [ lookupIndex allTypes t -- NOTE: Similar to Globals we store the pointee type.
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
              fArgTys = T.teParamTy (T.tePointeeTy (V.fType (V.symbolValue sig)))
              fArgs = map V.Arg fArgTys
              -- function local constant
              fconstants = sortOn V.cTy [c | c@(V.Constant{}) <- map V.symbolValue consts]
              -- the values the body can reference.
              bodyVals :: [V.Value]
              -- constants, globals, functions (because we order them that way)
              -- plus fargs and fconstants per function body ontop of which are
              -- the references generated by the instructions will be placed.
              bodyVals = constants ++ globals ++ functions ++ fArgs ++ fconstants
              nBodyVals = length bodyVals

              blockInstructions :: BasicBlock -> [I.Inst]
              blockInstructions (BasicBlock insts) = map snd insts
              blockInstructions (NamedBlock _ insts) = map snd insts

              -- instruction values (e.g. values generated by instructions)
              instVals = map (uncurry V.TRef) $ zip [t | Just t <- map I.instTy (concatMap blockInstructions bbs)] [0..]

              -- all values. This will be used to lookup indices for values in.
              allVals = bodyVals ++ instVals

              -- These are in FromBitCode as well. TODO: Refactor move BitMasks into a common file.
              inAllocMask = shift (1 :: Int) 5
              explicitTypeMask = shift (1 :: Int) 6
              swiftErrorMask = shift (1 :: Int) 7

              -- Relative Symbol lookup
              lookupRelativeSymbolIndex :: (Integral a)
                                        => [V.Value] -- ^ values prior to entering the instruction block
                                        -> [V.Value] -- ^ instruction values
                                        -> Int      -- ^ current instruction count
                                        -> V.Symbol   -- ^ the symbol to lookup
                                        -> a
              lookupRelativeSymbolIndex vs ivs iN s = fromIntegral $ vN + iN - lookupIndex vals v
                where v = V.symbolValue s
                      vN = length vs
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
              -- TODO: If we want to support InAlloca, we need to extend Alloca. For now we will not set the flag.
              mkInstRec n (I.Alloca t s a) = mkRec FC.INST_ALLOCA [ lookupIndex allTypes (T.tePointeeTy t)
                                                                  , lookupIndex allTypes . ty . V.symbolValue $ s
                                                                  , lookupIndex allVals (V.symbolValue s)
                                                                  , explicitTypeMask .|. bitWidth a
                                                                  ]
              -- TODO: Support Volatile flag
              mkInstRec n (I.Load _ s a) = mkRec FC.INST_LOAD [ lookupRelativeSymbolIndex' n s
                                                              , lookupIndex allTypes . T.tePointeeTy . ty . V.symbolValue $ s
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
              mkInstRec n (I.Call _ fn args) = mkRec FC.INST_CALL $ [ (0 :: Int) -- Fix PARAMATTR
                                                                               , fromEnum' (V.fCallingConv (V.symbolValue fn)) .|. (setBit 0 (fromEnum' CALL_EXPLICIT_TYPE))
                                                                               , lookupIndex allTypes fnTy
                                                                               , lookupRelativeSymbolIndex' n fn
                                                                               ] ++ map (lookupRelativeSymbolIndex' n) args
                                                where
                                                  -- We encode the result type of a call in the first
                                                  -- position of Call.  The (explicitly typed) call
                                                  -- instruction however wants to know what the type of
                                                  -- the called function is.  The function type though is
                                                  -- a pointer, hence we need to extract the underlying
                                                  -- type from the pointer that the function type is.
                                                  fnTy = T.tePointeeTy (V.fType (V.symbolValue fn))

              mkInstRec n (I.Cmp2 _ lhs rhs pred) = mkRec FC.INST_CMP2 [ lookupRelativeSymbolIndex' n lhs
                                                                                 , lookupRelativeSymbolIndex' n rhs
                                                                                 , fromEnum' pred :: Int
                                                                                 ]

              mkInstRec n (I.Gep ty inbounds base idxs) = mkRec FC.INST_GEP $ [ (bool inbounds) :: Int
                                                                                        , lookupIndex allTypes ty]
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
              mkInstRecFold (n, codes) inst = case I.instTy inst of
                Just _ -> (n+1,mkInstRec n inst:codes)
                Nothing -> (n, mkInstRec n inst:codes)

-- X = [NBlock 13 [NRec 1 [65,80,80,76,69,95,49,95,55,48,51,46,48,46,51,49,95,48],NRec 2 [0]]
--     ,NBlock 8
--      [NRec 1 [1]
--      ,NBlock 10 [NRec 3 [1,4294967295,0,18,0,26,0,33,4,100,105,115,97,98,108,101,45,116,97,105,108,45,99,97,108,108,115,0,102,97,108,115,101,0,4,108,101,115,115,45,112,114,101,99,105,115,101,45,102,112,109,97,100,0,102,97,108,115,101,0,4,110,111,45,102,114,97,109,101,45,112,111,105,110,116,101,114,45,101,108,105,109,0,116,114,117,101,0,3,110,111,45,102,114,97,109,101,45,112,111,105,110,116,101,114,45,101,108,105,109,45,110,111,110,45,108,101,97,102,0,4,110,111,45,105,110,102,115,45,102,112,45,109,97,116,104,0,102,97,108,115,101,0,4,110,111,45,110,97,110,115,45,102,112,45,109,97,116,104,0,102,97,108,115,101,0,4,115,116,97,99,107,45,112,114,111,116,101,99,116,111,114,45,98,117,102,102,101,114,45,115,105,122,101,0,56,0,4,116,97,114,103,101,116,45,99,112,117,0,99,111,114,101,50,0,4,116,97,114,103,101,116,45,102,101,97,116,117,114,101,115,0,43,99,120,49,54,44,43,102,120,115,114,44,43,109,109,120,44,43,115,115,101,44,43,115,115,101,50,44,43,115,115,101,51,44,43,115,115,115,101,51,0,4,117,110,115,97,102,101,45,102,112,45,109,97,116,104,0,102,97,108,115,101,0,4,117,115,101,45,115,111,102,116,45,102,108,111,97,116,0,102,97,108,115,101,0]
--                 ,NRec 3 [2,4294967295,4,100,105,115,97,98,108,101,45,116,97,105,108,45,99,97,108,108,115,0,102,97,108,115,101,0,4,108,101,115,115,45,112,114,101,99,105,115,101,45,102,112,109,97,100,0,102,97,108,115,101,0,4,110,111,45,102,114,97,109,101,45,112,111,105,110,116,101,114,45,101,108,105,109,0,116,114,117,101,0,3,110,111,45,102,114,97,109,101,45,112,111,105,110,116,101,114,45,101,108,105,109,45,110,111,110,45,108,101,97,102,0,4,110,111,45,105,110,102,115,45,102,112,45,109,97,116,104,0,102,97,108,115,101,0,4,110,111,45,110,97,110,115,45,102,112,45,109,97,116,104,0,102,97,108,115,101,0,4,115,116,97,99,107,45,112,114,111,116,101,99,116,111,114,45,98,117,102,102,101,114,45,115,105,122,101,0,56,0,4,116,97,114,103,101,116,45,99,112,117,0,99,111,114,101,50,0,4,116,97,114,103,101,116,45,102,101,97,116,117,114,101,115,0,43,99,120,49,54,44,43,102,120,115,114,44,43,109,109,120,44,43,115,115,101,44,43,115,115,101,50,44,43,115,115,101,51,44,43,115,115,115,101,51,0,4,117,110,115,97,102,101,45,102,112,45,109,97,116,104,0,102,97,108,115,101,0,4,117,115,101,45,115,111,102,116,45,102,108,111,97,116,0,102,97,108,115,101,0]]
--      ,NBlock 9 [NRec 2 [1],NRec 2 [2]]
--      ,NBlock 17 [NRec 1 [14],NRec 7 [8],NRec 11 [13,0],NRec 8 [1,0],NRec 7 [32],NRec 8 [0,0],NRec 8 [4,0],NRec 21 [0,3,3,5],NRec 8 [6,0],NRec 21 [1,3,4],NRec 8 [8,0],NRec 16 [],NRec 8 [3,0],NRec 8 [5,0],NRec 2 []]
--      ,NRec 2 [120,56,54,95,54,52,45,97,112,112,108,101,45,109,97,99,111,115,120,49,48,46,49,49,46,48]
--      ,NRec 3 [101,45,109,58,111,45,105,54,52,58,54,52,45,102,56,48,58,49,50,56,45,110,56,58,49,54,58,51,50,58,54,52,45,83,49,50,56]
--      ,NRec 7 [1,3,4,9,1,0,0,0,1,0,0,0]
--      ,NRec 8 [6,0,0,0,1,0,0,0,0,0,0,0,0,0,0]
--      ,NRec 8 [8,0,1,0,2,0,0,0,0,0,0,0,0,0,0]
--      ,NRec 13 [470]
--      ,NBlock 11 [NRec 1 [1],NRec 9 [104,101,108,108,111,32,119,111,114,108,100,10],NRec 1 [3],NRec 4 [2],NRec 4 [4]]
--      ,NBlock 15 [NRec 2 [3,4]
--                 ,NRec 1 [80,73,67,32,76,101,118,101,108]
--                 ,NRec 2 [3,5]
--                 ,NRec 3 [1,2,3]
--                 ,NRec 1 [65,112,112,108,101,32,76,76,86,77,32,118,101,114,115,105,111,110,32,55,46,51,46,48,32,40,99,108,97,110,103,45,55,48,51,46,48,46,51,49,41]
--                 ,NRec 3 [5]
--                 ,NRec 4 [108,108,118,109,46,109,111,100,117,108,101,46,102,108,97,103,115]
--                 ,NRec 10 [3]
--                 ,NRec 4 [108,108,118,109,46,105,100,101,110,116]
--                 ,NRec 10 [5]]
--      ,NBlock 15 [NRec 6 [0,100,98,103]
--                 ,NRec 6 [1,116,98,97,97]
--                 ,NRec 6 [2,112,114,111,102]
--                 ,NRec 6 [3,102,112,109,97,116,104]
--                 ,NRec 6 [4,114,97,110,103,101]
--                 ,NRec 6 [5,116,98,97,97,46,115,116,114,117,99,116]
--                 ,NRec 6 [6,105,110,118,97,114,105,97,110,116,46,108,111,97,100]
--                 ,NRec 6 [7,97,108,105,97,115,46,115,99,111,112,101]
--                 ,NRec 6 [8,110,111,97,108,105,97,115]
--                 ,NRec 6 [9,110,111,110,116,101,109,112,111,114,97,108]
--                 ,NRec 6 [10,108,108,118,109,46,109,101,109,46,112,97,114,97,108,108,101,108,95,108,111,111,112,95,97,99,99,101,115,115]
--                 ,NRec 6 [11,110,111,110,110,117,108,108]
--                 ,NRec 6 [12,100,101,114,101,102,101,114,101,110,99,101,97,98,108,101]
--                 ,NRec 6 [13,100,101,114,101,102,101,114,101,110,99,101,97,98,108,101,95,111,114,95,110,117,108,108]
--                 ,NRec 6 [14,109,97,107,101,46,105,109,112,108,105,99,105,116]
--                 ,NRec 6 [15,117,110,112,114,101,100,105,99,116,97,98,108,101]
--                 ,NRec 6 [16,105,110,118,97,114,105,97,110,116,46,103,114,111,117,112]
--                 ,NRec 6 [17,97,108,105,103,110]]
--      ,NBlock 12 [NRec 1 [1]
--                 ,NBlock 11 [NRec 1 [3]
--                            ,NRec 2 []
--                            ,NRec 1 [4]
--                            ,NRec 20 [1,2,0,3,8,3,8]]
--                 ,NRec 19 [3,3,4,67]
--                 ,NRec 19 [5,3,4,68]
--                 ,NRec 44 [2,6,3,0]
--                 ,NRec 44 [1,5,4,0]
--                 ,NRec 34 [0,32768,8,10,3]
--                 ,NRec 10 [5]
--                 ,NBlock 14 [NRec 1 [7,97,114,103,118]
--                            ,NRec 1 [6,97,114,103,99]]]
--      ,NBlock 14 [NRec 1 [2,112,114,105,110,116,102]
--                 ,NRec 3 [1,448,109,97,105,110]
--                 ,NRec 1 [0,46,115,116,114]]]]
