{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns  #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase    #-}
module Data.BitCode.LLVM.FromBitCode where

import Data.Bits (testBit, shift, (.|.), (.&.), complement, FiniteBits)
import Data.Word (Word64)

import Control.Monad (when, unless, foldM, foldM_)

import Data.BitCode (NBitCode(..), normalize, records, blocks, lookupBlock, lookupRecord)
import qualified Data.BitCode as BC
import Data.BitCode.LLVM
import Data.BitCode.LLVM.Reader.Monad
import Data.BitCode.LLVM.ParamAttr
import Data.BitCode.LLVM.IDs.Blocks as B
import qualified Data.BitCode.LLVM.Codes.Identification as IC
import Data.BitCode.LLVM.Codes.AttributeKind
import Data.BitCode.LLVM.Codes.Attribute
import Data.BitCode.LLVM.Codes.ValueSymtab
import Data.BitCode.LLVM.Codes.Constants
import Data.BitCode.LLVM.Codes.Metadata as MD
import Data.BitCode.LLVM.Codes.Function as FC

import Data.BitCode.LLVM.Value      as V
import Data.BitCode.LLVM.Type       as T
import Data.BitCode.LLVM.Instruction as I
import Data.BitCode.LLVM.Function   as F
import Data.BitCode.LLVM.Metadata
import Data.BitCode.LLVM.Codes.Type as TC
import Data.BitCode.LLVM.Codes.Module as M
import Data.Maybe (catMaybes, fromMaybe)

import qualified Data.BitCode.LLVM.Linkage as Linkage
import qualified Data.BitCode.LLVM.Visibility as Visibility
import qualified Data.BitCode.LLVM.ThreadLocalMode as ThreadLocalMode
import qualified Data.BitCode.LLVM.StorageClass as DLLStorageClass


import qualified Data.BitCode.LLVM.Flags as Flags

import Data.BitCode.LLVM.Opcodes.Binary as BinOp
import qualified Data.BitCode.LLVM.Opcodes.Cast as CastOp

import Debug.Trace

-- Conceptuall we take bitcode and interpret it as LLVM IR.
-- This should result in a single module.

parseAttr :: [NBitCode] -> LLVMReader ()
parseAttr = mapM_ parseAttr . records
  where parseAttr :: (AttributeCode, [BC.Val]) -> LLVMReader ()
        parseAttr (PARAMATTR_CODE_ENTRY, gidxs) = tellParamattr $ map fromIntegral gidxs
        parseAttr (c,_) = fail $ "PARAMATTR: code: " ++ show c  ++ "not (yet) supported"

-- | Documentation on this is pretty spotty.
--
-- Best so far is http://llvm.org/viewvc/llvm-project/llvm/trunk/lib/Bitcode/Reader/BitcodeReader.cpp?view=diff&r1=174848&r2=174849&pathrev=174849
--
-- ops: [goupId, idx, <flag>...] where
--       idx = 0 -> return value attributes.
--       idx = 2^32-1 -> function attributes
--       idx = n -> n'th argument attribute.
-- flag: 0,n -> AttributeKind(n)
--       1,KIND_ALIGNMENT,n -> AlignmentAttr += n
--       1,_,n              -> StackAlignmentAttr += n
--       4,...,0,...0       -> String: Kind, Value
--       _,...,0            -> String: Kind (empty value)
parseAttrGroup :: [NBitCode] -> LLVMReader ()
parseAttrGroup = mapM_ parseAttrGroup . records
  where parseAttrGroup :: (AttributeCode, [BC.Val]) -> LLVMReader ()
        parseAttrGroup (PARAMATTR_GRP_CODE_ENTRY, (n:vals)) = tellParamattrGroup (fromIntegral n, parseAttrGroupEntry vals)
        parseAttrGroup (c,_) = fail $ "PARAMATTR_GROUP: code: " ++ show c ++ "not (yet) supported"

        parseAttrGroupEntry :: [BC.Val] -> ParamAttrGroupEntry
        parseAttrGroupEntry (t:vs) = GroupEntry (grpIdx t) (go vs)
          where grpIdx :: BC.Val -> ParamAttrGroupIdx
                grpIdx 0          = Data.BitCode.LLVM.ParamAttr.Ret
                grpIdx 0xffffffff = Fun
                grpIdx n          = Param n
                go :: [BC.Val] -> [ParamAttrEntry]
                go [] = []
                go (0:n:vs) = Kind (toEnum . fromIntegral $ n):go vs
                go (1:a:n:vs) | a == fromIntegral (fromEnum ATTR_KIND_ALIGNMENT) = Align n:go vs
                              | otherwise                         = StackAlign n:go vs
                go (4:vs) = let key = map (toEnum . fromIntegral) $ takeWhile (/= 0) vs
                                val = map (toEnum . fromIntegral) $ takeWhile (/= 0) (drop (length key + 1) vs)
                            in Pair key (Just val):go (drop (length key + length val + 2) vs)
                go (_:vs) = let key = map (toEnum . fromIntegral) $ takeWhile (/= 0) vs
                            in Pair key Nothing:go (drop (length key + 1) vs)



-- 12 - Function
-- see below

-- 13 - Identification
parseIdent :: [NBitCode] -> LLVMReader Ident
parseIdent body = let Just s    = lookupRecord IC.STRING body
                      Just [e]  = lookupRecord IC.EPOCH  body
                  in return $ Ident (map toEnum' s) (toEnum' e)

-- parseBlock (BC.Block code _ body) | code == fromEnum VALUE_SYMTAB -- 14
--   = let parseValue :: BC.Record -> (Int,ValueSymbolEntry)
--         parseValue (code, vals) = case toEnum code of
--           VST_CODE_ENTRY   -> let (idx:vs) = vals        in (fromIntegral idx, Entry $ map (toEnum . fromIntegral) vs)
--           VST_CODE_FNENTRY -> let (idx:offset:vs) = vals in (fromIntegral idx, FnEntry (fromIntegral offset) $ map (toEnum . fromIntegral) vs)
--     in pure $ ValueSymTab . map parseValue . catMaybes . map toRecord $ body

-- 15 - Metadata
-- 16 - MetadataAttachment
-- Plan of attack
-- Normalize Records (Abbrev, and Unabbrev into Record -- how do we handle the *richer* types?[1])
-- We somehow need to drop `len` ops from it though. Probably best to have some special Field type
-- so we can filter non-control ops out.


-- [1] could turn Char into Word64 through Ord; would then have to reinterpret them accordingly.


-- 17 - Type (new)
-- Needs TypeTableReader and TypeTableWriter
-- NOTE: We do some extra dance because Opaque and Named Structures are emitted as
-- a record that holds the name (STRUCT_NAME) and then a record that consumes that
-- name (OPAQUE and STRUCT_NAMED).
-- TODO: Add assert that length <$> askTypeList is equal to the value obtained from the numentry record.
parseTypes :: [NBitCode]                         -> LLVMReader ()
parseTypes = foldM_ parseType Nothing . records
  where parseType :: Maybe String -> (Type,[BC.Val])                  -> LLVMReader (Maybe String)
        parseType name = \case
          -- ignore number of entries record.
          (NUMENTRY,    [n]                    ) -> pure name
          (VOID,        []                     ) -> tellType Void                                                                     >> pure name
          (FLOAT,       []                     ) -> tellType T.Float                                                                  >> pure name
          (DOUBLE,      []                     ) -> tellType Double                                                                   >> pure name
          (LABEL,       []                     ) -> tellType Label                                                                    >> pure name
          (OPAQUE,      []                     )
            | (Just n) <- name                   -> tellType (Opaque n)                                                               >> pure Nothing
            | otherwise                          -> fail "Opaque needs a name!"
          (INTEGER,     [width]                ) -> (tellType $ T.Int width)                                                          >> pure name
          (POINTER,     [tyId, width]          ) -> askType tyId >>= tellType . Ptr width                                             >> pure name
          (POINTER,     [tyId]                 ) -> askType tyId >>= tellType . Ptr 0                                                 >> pure name
          (HALF,        []                     ) -> tellType Half                                                                     >> pure name
          (ARRAY,       [numElts, eltTyId]     ) -> askType eltTyId >>= tellType . T.Array numElts                                    >> pure name
          (VECTOR,      [numElts, eltTyId]     ) -> askType eltTyId >>= tellType . T.Vector numElts                                   >> pure name
          (X86_FP80,    []                     ) -> tellType X86Fp80                                                                  >> pure name
          (FP128,       []                     ) -> tellType Fp128                                                                    >> pure name
          (TC.METADATA, []                     ) -> tellType T.Metadata                                                               >> pure name
          (X86_MMX,     []                     ) -> tellType X86Mmx                                                                   >> pure name
          (STRUCT_ANON, (isPacked:eltTyIds)    ) -> mapM askType eltTyIds >>= tellType . StructAnon (isPacked /= 0)                   >> pure name
          (STRUCT_NAME, ops                    ) -> pure (pure (toString ops))
          (STRUCT_NAMED,(isPacked:eltTyIds)    )
            | (Just n) <- name                   -> mapM askType eltTyIds >>= tellType . StructNamed n (isPacked /= 0)                >> pure Nothing
            | otherwise                          -> fail "Named Struct needs a name!"
          (TC.FUNCTION, (vararg:retTy:paramTys)) -> T.Function (vararg /= 0) <$> askType retTy <*> mapM askType paramTys >>= tellType >> pure name
          (TOKEN,       []                     ) -> tellType Token                                                                    >> pure name
          (code,        ops                    ) -> fail $ "Can not handle type: " ++ show code ++ " with ops: " ++ show ops
        toString :: (Integral a) => [a]          -> String
        toString = map (toEnum . fromIntegral)

-- 11 - Constants
-- Parse Constnats and add them to the valueList

-- | toSigned helper. Bitcode does doesn't encode signed
-- values actually. But if a signed value needs to be
-- encoded (an this can only be application specific)
-- it is shifted by one and the low bit is set if
-- negative.  This function reverses that encoding.
toSigned :: (FiniteBits a) => a -> a
toSigned v | testBit v 0 = complement (shift v (-1))
           | otherwise   = shift v (-1)

-- | Parse constants.
parseConstants :: [NBitCode] -> LLVMReader ()
parseConstants = foldM_ parseConstant undefined . records
  where parseConstant :: Ty -> (Constant,[BC.Val]) -> LLVMReader Ty
        parseConstant ty = \case
          (CST_CODE_SETTYPE, [tyId]) -> askType tyId
          (CST_CODE_NULL,    []    ) -> add $ mkConst V.Null
          (CST_CODE_UNDEF,   []    ) -> add $ mkConst V.Undef
          (CST_CODE_INTEGER, [val] ) -> add $ mkConst (V.Int (toSigned val))
          (CST_CODE_WIDE_INTEGER, vals) -> add $ mkConst (V.WideInt (map toSigned vals))
-- TODO: how do we interpret a Word64 as a FPVal?
--          CST_CODE_FLOAT   -> let [val] = vals
--                              in (++(ty, C.Float val)) <$> go ty rs
          (CST_CODE_AGGREGATE, valIds)
          -- XXX: We *assume*, but do not verify that the types of the askValue's actually match those of the structure.
            | T.StructAnon{}   <- ty -> add =<< mkConst <$> (V.Struct <$> mapM askValue valIds)
            | T.StructNamed{}  <- ty -> add =<< mkConst <$> (V.Struct <$> mapM askValue valIds)
            | T.Array _ t      <- ty -> add =<< mkConst <$> (V.Array <$> mapM askValue valIds)
            | T.Vector _ t     <- ty -> add =<< mkConst <$> (V.Vector <$> mapM askValue valIds)
            | otherwise              -> add $ mkConst V.Undef
          (CST_CODE_STRING, vals)  -> add $ mkConst (V.String $ map toEnum' vals)
          (CST_CODE_CSTRING, vals) -> add $ mkConst (V.CString $ map toEnum' vals)
          (CST_CODE_CE_BINOP, [code, lhs, rhs]) -> add =<< mkConst <$> (V.BinOp (toEnum' code) <$> askValue lhs <*> askValue rhs)
          (CST_CODE_CE_CAST, [ code, tyId, valId ]) -> add =<< mkConst <$> (V.Cast (toEnum' code) <$> askType tyId <*> askValue valId)
          (CST_CODE_CE_GEP, vals)  -> add $ mkConst (V.Gep vals)
-- TODO: CST_CODE_CE_SELECT
--       CST_CODE_CE_EXTRACTELT
--       CST_CODE_CE_INSERTELT
--       CST_CODE_CE_SHUFFLEVEC
--       CST_CODE_CE_CMP
--       CST_CODE_CE_INLINEASM_OLD
--       CST_CODE_CE_SHUFVEC_EX
          (CST_CODE_CE_INBOUNDS_GEP, (v:vs))
            -- either [t, [tyId, valId, ...]]
            | length vs `mod` 2 == 0 -> do
                t <- askType v
                add =<< mkConst . V.InboundsGep t <$> getTypedSymbols vs
            | otherwise -> do
                let t = Ptr 0 Void -- nullptr
                add =<< mkConst . V.InboundsGep t <$> getTypedSymbols vs
-- TODO: CST_CODE_BLOCKADDRESS
--       CST_CODE_DATA
--       CST_CODE_INLINEASM
          (c,vs) -> error $ "Code: " ++ show c ++ " not supported; values: " ++ show vs ++ "; current type: " ++ show ty
          where mkConst :: Const -> Value
                mkConst = Constant ty
                add :: Value -> LLVMReader Ty
                add val = tellValue val >> pure ty
                -- WARNING - TODO: Converting Word64 to possible Int(32).
                toSigned :: Word64 -> Int
                toSigned w = fromIntegral $ case (testBit w 0, shift w (-1)) of
                  (True,  v) -> -v
                  (False, v) ->  v
                getTypedSymbols :: [Word64] -> LLVMReader [Symbol]
                getTypedSymbols [] = pure []
                getTypedSymbols (tId:vId:vs) = do
                  t <- askType tId
                  v <- askValue vId
                  -- TODO: check that the type of v matches t.
                  (v:) <$> getTypedSymbols vs

-- Metadata kind 22
parseMetadataKinds :: [NBitCode] -> LLVMReader ()
parseMetadataKinds = mapM_ parseMetadataKind . records
  where parseMetadataKind :: (MD.Metadata,[BC.Val]) -> LLVMReader ()
        parseMetadataKind = \case
          (MD.METADATA_KIND, (idx:vals)) -> tellMetadataKind ((fromIntegral idx), map toEnum' vals)
          _ -> pure () -- ignore.
-- Metadata 15
parseMetadata :: [NBitCode] -> LLVMReader ()
parseMetadata = mapM_ parseMetadata . records
  where parseMetadata :: (MD.Metadata, [BC.Val]) -> LLVMReader ()
        parseMetadata = \case
          (MD.METADATA_STRING, vals) -> tellMetadata $ MDString (map toEnum' vals)
          (MD.METADATA_VALUE, [tyId,val]) -> askType tyId >>= \ty -> tellMetadata $ MDValue ty val
          (MD.METADATA_NODE, mdIds) -> tellMetadata =<< MDNode <$> mapM askMetadata (map pred mdIds)
          (MD.METADATA_NAME, vals)  -> tellMetadata $ MDName (map toEnum' vals)
          (MD.METADATA_DISTINCT_NODE, mdIds) -> tellMetadata =<< MDDistinctNode <$> mapM askMetadata (map pred mdIds)
          (MD.METADATA_LOCATION, [distinct, line, col, scope, inlinedAt]) -> tellMetadata $ MDLocation (distinct /= 0) line col scope inlinedAt
          -- this is such a weird encoding.
          -- basically emit the name. And then emit a named node, to use the just emitted name.
          (MD.METADATA_NAMED_NODE, mIds) -> popMetadata >>= \(MDName name) -> MDNamedNode name <$> mapM askMetadata mIds >>= tellMetadata
          (MD.METADATA_KIND, (idx:vals)) -> tellMetadataKind ((fromIntegral idx), map toEnum' vals)
          (c, ops) -> fail $ "Unsupported metadata: " ++ show c ++ " with ops: " ++ show ops

-- * module codes

parseVersion :: [BC.Val] -> Word64
parseVersion [v] = v

parseTriple :: [BC.Val] -> String
parseTriple = map toEnum'

parseDataLayout :: [BC.Val] -> String
parseDataLayout = map toEnum'

parseGlobalVar :: [BC.Val] -> LLVMReader ()
parseGlobalVar vals
  | length vals < 6 = error $ "Global Var must have at least six operands. " ++ show vals ++ " given."
  | [ ptrTyId, isConst, initId, linkage, paramAttrId, section ] <- vals = do
      ty <- askType ptrTyId
      unless (testBit isConst 1) $ fail "non-explicit type global vars are not (yet) supported"
      let addressSpace = shift isConst (-2)
          initVal      = if initId /= 0 then Just (FwdRef (initId -1)) else Nothing
          linkage'     = toEnum' linkage
          storageClass = upgradeDLLImportExportLinkage linkage'
          comdat       = 0 -- XXX. the Reader does some weird hasImplicitComdat for older bitcode.
                           -- we'll ignore this here for now.
      tellValue $ Global (Ptr 0 ty) (testBit isConst 0) addressSpace initVal
                  linkage' paramAttrId section Visibility.Default ThreadLocalMode.NotThreadLocal
                  False False storageClass comdat
  | [ ptrTyId, isConst, initId, linkage
    , paramAttrId, section, visibility, threadLocalMode
    , unnamedAddr, externallyInitialized, storageClass
    , comdat ] <- vals = do
      ty <- askType ptrTyId
      -- TODO: isConst has bit 0 set if const. bit 1 if explicit type. We only handle explicit type so far.
      unless (testBit isConst 1) $ fail "non-explicit type global vars are not (yet) supported"
      let addressSpace = shift isConst (-2)
          initVal      = if initId /= 0 then Just (FwdRef (initId - 1)) else Nothing

      tellValue $ Global (Ptr 0 ty) (testBit isConst 0) addressSpace initVal
                         (toEnum' linkage) paramAttrId section (toEnum' visibility) (toEnum' threadLocalMode)
                         (unnamedAddr /= 0) (externallyInitialized /= 0) (toEnum' storageClass) comdat
  | otherwise = error $ "unhandled parseGlobalVar with values: " ++ (show vals)

parseFunctionDecl :: [BC.Val] -> LLVMReader ()
parseFunctionDecl
  [ tyId, cconv, isProto, linkage
  , paramAttrId, alignment, section, visibility, gc
  , unnamedAddr, prologueData, storageClass, comdat
  , prefixData, personality ]
  = do
  ty <- askType tyId
  tellValue $ V.Function (Ptr 0 ty) (toEnum' cconv) (isProto /= 0) (toEnum' linkage)
                         paramAttrId alignment section (toEnum' visibility) gc
                         (unnamedAddr /= 0) prologueData (toEnum' storageClass)
                         comdat prefixData personality


upgradeDLLImportExportLinkage :: Linkage.Linkage -> DLLStorageClass.DLLStorageClass
upgradeDLLImportExportLinkage = \case
  Linkage.WeakODR   -> DLLStorageClass.DLLImport
  Linkage.Appending -> DLLStorageClass.DLLExport
  _                 -> DLLStorageClass.Default

parseAlias :: Bool     -- ^ New Alias
           -> [BC.Val] -- Values
           -> LLVMReader ()

-- old alias, without addrSpace explicitly given.
parseAlias False [ tyId, valId, linkage, visibility, storageClass, threadLocalMode, unnamedAddr ] = do
  nVals <- length <$> askValueList
  nTypes <- length <$> askTypeList
  ty@(Ptr addrSpace _) <- askType tyId
  -- val <- askValue valId
  tellValue $ Alias ty addrSpace (Unnamed (V.FwdRef valId)) (toEnum' linkage) (toEnum' visibility) (toEnum' threadLocalMode) (unnamedAddr /= 0) (toEnum' storageClass)
parseAlias False [ tyId, valId, linkage, visibility, storageClass, threadLocalMode ]
  = parseAlias False [ tyId, valId, linkage, visibility, storageClass, threadLocalMode, 0 ]
parseAlias False [ tyId, valId, linkage, visibility, storageClass ]
  = parseAlias False [ tyId, valId, linkage, visibility, storageClass, (fromIntegral (fromEnum ThreadLocalMode.NotThreadLocal)) ]
parseAlias False [ tyId, valId, linkage, visibility ]
  = parseAlias False [ tyId, valId, linkage, visibility, (fromIntegral . fromEnum $ upgradeDLLImportExportLinkage (toEnum' linkage)) ]
parseAlias False [ tyId, valId, linkage ]
  = parseAlias False [ tyId, valId, linkage, (fromIntegral (fromEnum Visibility.Default)) ]
-- new
parseAlias True [ tyId, addrSpace, valId, linkage, visibility, storageClass, threadLocalMode, unnamedAddr ] = do
  ty <- askType tyId
  val <- askValue valId
  tellValue $ Alias ty addrSpace val (toEnum' linkage) (toEnum' threadLocalMode) (toEnum' visibility) (unnamedAddr /= 0) (toEnum' storageClass)
parseAlias True [ tyId, addrSpace, valId, linkage, visibility, storageClass, threadLocalMode ]
  = parseAlias True [ tyId, addrSpace, valId, linkage, visibility, storageClass, threadLocalMode, 0 ]
parseAlias True [ tyId, addrSpace, valId, linkage, visibility, storageClass ]
  = parseAlias True [ tyId, addrSpace, valId, linkage, visibility, storageClass, (fromIntegral (fromEnum ThreadLocalMode.NotThreadLocal)) ]
parseAlias True [ tyId, addrSpace, valId, linkage, visibility ]
  = parseAlias True [ tyId, addrSpace, valId, linkage, visibility, (fromIntegral . fromEnum $ upgradeDLLImportExportLinkage (toEnum' linkage)) ]
parseAlias True [ tyId, addrSpace, valId, linkage ]
  = parseAlias True [ tyId, addrSpace, valId, linkage, (fromIntegral (fromEnum Visibility.Default)) ]

-- helper
toEnum' :: (Integral a, Enum e) => a -> e
toEnum' = toEnum . fromIntegral

parseTopLevel :: [NBitCode] -> LLVMReader (Maybe Ident, Module)
parseTopLevel bs = do
  ident <- case lookupBlock IDENTIFICATION bs of
    Just b -> Just <$> parseIdent b
    Nothing -> return Nothing

  let Just moduleBlock = lookupBlock MODULE bs
  mod <- parseModule moduleBlock
  return (ident, mod)

resolveFwdRefs :: [Symbol] -> [Symbol]
resolveFwdRefs s = map (fmap' resolveFwdRef') s
  where
    -- TODO: Maybe Symbol should be more generic? Symbol a,
    --       then we could have Functor Symbol.
    fmap' :: (Value -> Value) -> Symbol -> Symbol
    fmap' f (Named s v) = Named s (f v)
    fmap' f (Unnamed v) = Unnamed (f v)
    resolveFwdRef' :: Value -> Value
    resolveFwdRef' g@(Global{..}) = case gInit of
      Just (FwdRef id) -> g { gInit = Just $ symbolValue (s !! (fromIntegral id)) }
      _                -> g
    -- resolve fws refs only for globals for now.
    resolveFwdRef' x = x

-- | Parse a module from a set of blocks (the body of the module)
parseModule :: [NBitCode] -> LLVMReader Module
parseModule bs = do
  let Just version = parseVersion     <$> lookupRecord VERSION bs
      triple  = parseTriple           <$> lookupRecord TRIPLE bs
      layout  = parseDataLayout       <$> lookupRecord DATALAYOUT bs
      vst     = parseSymbolValueTable <$> lookupBlock VALUE_SYMTAB bs

  traceM "Parsing Blocks"
  flip mapM_  bs $ \case
    (NBlock c bs') -> parseModuleBlock (toEnum c, bs')
    (NRec   c vs)  -> parseModuleRecord (toEnum c, vs)

  traceM "Parsing VST"
  -- update values with symbols
  case vst of
    Just vst -> tellValueSymbolTable vst
    Nothing -> pure ()

  -- update forward references
  resolveFwdRefs <$> askValueList >>= tellValueList

  -- obtain a snapshot of all current values
  values <- askValueList

  traceM "Parsing Decls"

  let functionDefs = [f | f@(Named _ (V.Function {..})) <- values, not fIsProto] ++
                     [f | f@(Unnamed (V.Function {..})) <- values, not fIsProto]
      functionDecl = [f | f@(Named _ (V.Function {..})) <- values, fIsProto ] ++
                     [f | f@(Unnamed (V.Function {..})) <- values, fIsProto ]
  (unless (length functionDefs == length functionBlocks)) $ fail $ "#functionDecls (" ++ show (length functionDefs) ++ ") does not match #functionBodies (" ++ show (length functionBlocks) ++ ")"

  traceM "Parsing Functions"

  fns <- mapM parseFunction (zip functionDefs functionBlocks)

  return $ Module version triple layout values functionDecl fns
  where
    functionBlocks :: [[NBitCode]]
    functionBlocks = [bs' | (B.FUNCTION, bs') <- blocks bs ]
    symbolize :: [(Int, ValueSymbolEntry)] -> [Value] -> [Symbol]
    symbolize m = map (\(idx, val) -> case (lookup idx m) of
                          Just (Entry s) -> Named s val
                          Just (FnEntry _ s) -> Named s val
                          Nothing -> Unnamed val
                          ) . zip [0..]

-- | Parse value symbol table
parseSymbolValueTable :: [NBitCode] -> ValueSymbolTable
parseSymbolValueTable = foldl (\l x -> parseSymbolValue x:l) [] . filter f . records
  where parseSymbolValue :: (ValueSymtabCodes, [BC.Val]) -> (Int, ValueSymbolEntry)
        parseSymbolValue (VST_CODE_ENTRY,   (idx:vs)) = (fromIntegral idx, Entry $ map toEnum' vs)
        parseSymbolValue (VST_CODE_FNENTRY, (idx:offset:vs)) = (fromIntegral idx, FnEntry (fromIntegral offset) $ map toEnum' vs)
        f :: (ValueSymtabCodes, [BC.Val]) -> Bool
        f (VST_CODE_ENTRY, _) = True
        f (VST_CODE_FNENTRY, _) = True
        f _ = False

-- block ids
parseModuleBlock :: (ModuleBlockID, [NBitCode]) -> LLVMReader ()
parseModuleBlock (id,bs) = traceM ("parseModuleBlock " ++ show id) >> case (id,bs) of
  ({-  9 -}PARAMATTR, bs) -> parseAttr bs
  ({- 10 -}PARAMATTR_GROUP, bs) -> parseAttrGroup bs
  ({- 11 -}CONSTANTS, bs) -> parseConstants bs
  ({- 12 -}B.FUNCTION, bs) -> return () -- parsing of function bodies is handled differently.
  ({- 13 -}IDENTIFICATION, bs) -> return () -- this is not even part of the MODULE block. But alongside the module block.
  ({- 14 -}VALUE_SYMTAB, bs) -> return () -- TODO
  ({- 15 -}B.METADATA, bs) -> parseMetadata bs
  ({- 16 -}METADATA_ATTACHMENT_ID, bs) -> return () -- TODO
  ({- 17 -}TYPE_NEW, bs) -> parseTypes bs
  ({- 18 -}USELIST, bs) -> return () -- TODO
  ({- 19 -}MODULE_STRTAB, bs) -> return () -- TODO
  ({- 20 -}FUNCTION_SUMMARY, bs) -> return () -- TODO
  ({- 21 -}OPERAND_BUNDLE_TAGS, bs) -> return () -- TODO
  ({- 22 -}B.METADATA_KIND, bs) -> parseMetadataKinds bs
  c -> fail $ "Encounterd unhandled block: " ++ show c

parseModuleRecord :: (ModuleCode, [BC.Val]) -> LLVMReader ()
parseModuleRecord (id,bs) = traceM ("parseModuleRecord " ++ show id) >> case (id,bs) of
  ({-  1 -}VERSION, _) -> pure () -- ignore, it's being picked apart somewhere else.
  ({-  2 -}TRIPLE, _) -> pure () -- ignore
  ({-  3 -}DATALAYOUT, _) -> pure () -- ignore
  -- ({-  4 -}ASM, asm) -> -- unhandled
  ({-  5 -}SECTIONNAME, name) -> traceM $ "!! ignoring section name " ++ (map toEnum' name)
  -- ({-  6 -}DEPLIB, name) -- unhanlded, will be removed in 4.0 anyway.
  ({-  7 -}GLOBALVAR, vs) -> parseGlobalVar vs
  ({-  8 -}M.FUNCTION, vs) -> parseFunctionDecl vs
  ({-  9 -}ALIAS_OLD, vs) -> parseAlias False vs
  -- ({- 10 -}PURGEVALS, numvals) -- unhandled; no idea how to implement this without chaning to stream processing of the blocks.
  -- ({- 11 -}GCNAME, name) -- unhandled
  -- ({- 12 -}COMDAT, [ sectionKind, name ]) -- unhandled
  -- as we do not jump to the VST, we can safely ignore it here.
  ({- 13 -}VSTOFFSET, [ offset ]) -> traceM $ "!! ignoring VSTOffset " ++ show offset
  ({- 14 -}ALIAS, vs) -> parseAlias True vs
--  ({- 15 -}METADATA_VALUES, numvals)
  -- ignore others; e.g. we only need to parse the ones above in sequence to populate the valuetable properly.
  (id,ops) -> fail $ "Encountered unhandled record: " ++ show id ++ " with ops: " ++ show ops


-- | parsing a function block from bitcode.
-- function can contain their own set of
-- constants which are virtually added to
-- the values table. Similarly they have their
-- arguments put into the values table before
-- the body is parsed.
--
-- The LLVM Reader makes be believe, we can
-- expect to see the instruction records as
-- well as the following blocks:
-- Constants, VST, MetadataAttachment, Metadata,
-- Uselist.
--
-- So a Function consists of
-- - Constants (with maybe VST info)
-- - MetadataAttachment, Metadata -- let's ignore this for now.
-- - Uselist (?)
-- - [Instructions] -- where we basically need to use a temporary
--                     ValueList = GlobalValueList + Constants + Function Arguments.
--                     and reset it at the end of the function.

-- Function bodies should come in sequence of their declaration in the GV.
-- prototype functions are external.
--
parseFunction :: (Symbol, [NBitCode]) -> LLVMReader F.Function
parseFunction (f@(Named _ V.Function{..}), b) = do
  -- remember the size of the value list. We need to trim it back down after
  -- parsing; and might want to attach the new values to the constants of the Function.
  -- The same holds for metadata attachment.
  savedValueList <- askValueList
  savedVST       <- askValueSymbolTable
  -- Not sure what we do about Uselist yet.
  let Ptr _ (T.Function _ _ paramTys) = traceShowId $ fType
  -- put the decl header onto the valuelist.
  mapM_ (tellValue . Arg) paramTys
  nVals' <- length <$> askValueList
  -- let's parse all constants if any.
  mapM_ parseFunctionBlock (blocks b)

  case parseSymbolValueTable <$> lookupBlock VALUE_SYMTAB b of
    Just vst -> tellValueSymbolTable vst
    Nothing -> pure ()

  consts <- drop nVals' . resolveFwdRefs <$> askValueList
  -- parse the instructions
  -- the first basic block is going to be empty. As the body
  -- has to finish with a terminator, which adds a final empty
  -- BB to the front.
  (_:bbs,_) <- foldM foldHelper ([BasicBlock []],[]) (records b)
  -- reset the valueList to before we entered the
  -- function body, as they were local to
  tellValueList savedValueList
  tellValueSymbolTable savedVST
  return $ F.Function f consts (reverse bbs)

parseFunction ((Unnamed f), b) = parseFunction ((Named "dummy" f), b)
parseFunction _ = fail "Invalid arguments"


parseFunctionBlock :: (ModuleBlockID, [NBitCode]) -> LLVMReader ()
parseFunctionBlock = \case
  (CONSTANTS, b) -> parseConstants b
  (B.METADATA, b) -> parseMetadata b
  (B.METADATA_ATTACHMENT_ID, b) -> traceM ("Ignoring Metadata attachment: " ++ show b)
  (B.USELIST, b) -> traceM ("Cannot parse uselist yet (" ++ show b ++ ")") >> return ()
  _ -> pure ()

getRelativeVal :: (Integral a) => [Symbol] -> a -> LLVMReader Symbol
getRelativeVal refs n = do
  valueList <- askValueList
  pure $ reverse (valueList ++ refs) !! (fromIntegral n - 1)


-- TODO: filter out the `FUNC_CODE_DECLAREBLOCKS` in
--       the foldHelper. We can then simplify the
--       parseInst function to be of result type
--       LLVMReader Inst.
foldHelper :: ([BasicBlock],[Symbol]) -> (Instruction, [BC.Val]) -> LLVMReader ([BasicBlock],[Symbol])
foldHelper s@((BasicBlock insts):bbs,vs) instr = do
  i <- parseInst vs instr
  case i of
    Nothing -> return s
    Just i -> do let mref = Unnamed . flip TRef (length vs) <$> instTy i
                     vs'    = vs ++ [r | Just r <- [mref]]
                     insts' = insts ++ [(mref, i)]
                     bbs'   = (BasicBlock insts'):bbs
                 case isTerminator i of
                   True -> return ((BasicBlock []):bbs', vs')
                   False -> return                (bbs', vs')

parseInst :: [Symbol] -> (Instruction, [BC.Val]) -> LLVMReader (Maybe Inst)
parseInst rs = \case
  -- 1
  (DECLAREBLOCKS, _) -> pure Nothing -- ignore.
  -- 2
  (INST_BINOP, (lhs:rhs:code:flags)) -> traceShow flags $ do
    lhs <- getRelativeVal rs lhs
    rhs <- getRelativeVal rs rhs
    let opTy = ty (symbolValue lhs)
        code' = (toEnum' code) :: BinOp
        flags' = case flags of
          [] -> []
          [bitfield]
            | code' `elem` [ADD, SUB, MUL, SHL] -> map Flags.Overflow $ filter (testBit bitfield . fromEnum) [Flags.NO_UNSIGNED_WRAP, Flags.NO_SIGNED_WRAP]
            | code' `elem` [UDIV, SDIV, LSHR, ASHR] -> map Flags.Exact $ filter (testBit bitfield . fromEnum) [Flags.EXACT]
            | otherwise -> []

    return $ Just (I.BinOp opTy code' lhs rhs flags')
  -- 3
  (INST_CAST, [ valId, tyId, opCode ]) -> do
    val <- getRelativeVal rs valId
    ty  <- askType tyId
    let op =  toEnum' opCode
    return $ Just (I.Cast ty op val)
  -- 4
  -- (INST_GEP_OLD, vals)
  -- 5
  -- (INST_SELECT, vals)
  -- 6
  -- (INST_EXTRACTELT, vals)
  -- 7
  -- (INST_INSERTELT, vals)
  -- 8
  -- (INST_SHUFFLEVEC, vals)
  -- 9
  -- (INST_CMP, vals)
  -- 10
-- Even thought the documentaiton sais [ty [, val]], it's
-- actually [val] (or [val, ty] in case of fwd ref).
-- if [val] is empty. It' a Void return.
  (INST_RET, []) -> return . Just $ I.Ret Nothing
  (INST_RET, [valId]) -> do
    val <- Just <$> getRelativeVal rs valId
    return . Just $ I.Ret val
  -- 11
  (INST_BR, [bbN]) -> return . Just $ UBr bbN
  (INST_BR, [bbN, bbN', cond]) -> do
    cond' <- getRelativeVal rs cond
    return . Just $ Br cond' bbN bbN'
  -- 12
  -- (INST_SWITCH, vals)
  -- 13
  -- (INST_INVOKE, vals)
  -- 14 - Unused
  -- 15
  -- (INST_UNREACHABLE, [])
  -- 16
  -- (INST_PHI, (ty:val:[bbs]))
  -- 17, 18 - Unused
  -- 19
  (INST_ALLOCA, [ instty, opty, op, align ]) -> do
    iTy <- askType instty
    oTy <- askType opty
    val <- askValue op -- probably a constant.
    return . Just $  Alloca (Ptr 0 iTy) val (decodeAlign align)
      where decodeAlign :: Word64 -> Word64
            decodeAlign a = 2^((a .&. (complement inAllocMask .|. explicitTypeMask .|. swiftErrorMask)) - 1)
            inAllocMask = shift 1 5
            explicitTypeMask = shift 1 6
            swiftErrorMask = shift 1 7
  -- 20
  (INST_LOAD, [ op, opty, align, vol]) -> do
    oTy <- askType opty
    val <- getRelativeVal rs op
    return . Just $ Load oTy val (2^(align-1))
  -- 21, 22 - Unused
  -- 23
  -- (INST_VAARG, [ valistty, valist, instty ])
  -- 24
  -- (INST_STORE_OLD [ ptrty, ptr, val, align, vol])
  -- 25 - Unused
  -- 26
  -- (INST_EXTRACTVAL, ops)
  -- 27
  -- (INST_INSERTVAL, ops)
  -- 28
  (INST_CMP2, [lhs, rhs, pred]) -> do
    lhs' <- getRelativeVal rs lhs
    rhs' <- getRelativeVal rs rhs
    -- result type is:
    --  if lhs is vector of n -> Vector <i1 x n>
    --  else                  -> i1
    let oTy = case (ty lhs') of
          T.Vector n _ -> T.Vector n (T.Int 1)
          _ -> T.Int 1
    return . Just $ Cmp2 oTy lhs' rhs' (toEnum' pred)
  -- 29
  -- (INST_VSELECT, [ ty, opval, opval, predty, pred])
  -- 30
  -- (INST_INBOUNDS_GEP_OLD, ops)
  -- 31
  -- (INST_INDIRECTBR, (opty:ops))
  -- 32 - Unused
  -- 33
  -- (DEBUG_LOC_AGAIN, [])
  -- 34
  -- [paramattrs, cc[, fmf][, explfnty], fnid, arg0, arg1...]
  (INST_CALL, (paramattr:cc:ops)) -> do
    let (fmf, ops') = if testBit cc (fromEnum Flags.CALL_FMF) then (Just (head ops), tail ops) else (Nothing, ops)
    let (explFnTy, ops') = if testBit cc (fromEnum Flags.CALL_EXPLICIT_TYPE) then (Just (head ops), tail ops) else (Nothing, ops)
    let (fnid:args) = ops'
    fn <- getRelativeVal rs fnid

    fnTy <- case explFnTy of
      Just ty -> askType ty
      Nothing -> pure $ tePointeeTy (fType (symbolValue fn))

    args <- mapM (getRelativeVal rs) args
    return . Just $ Call (teRetTy fnTy) fn args
  -- 35
  -- (DEBUG_LOC)
  -- 36
  -- (INST_FENCE, [ordering, synchscope])
  -- 37
  -- (INST_CMPXCHG_OLD, [ptrty, ptr, cmp, new, align, vol, ordering, synchscope])
  -- 38
  -- (INST_ATOMICRMW, [ptrty, ptr, val, operation, align, vol,ordering, synchscope])
  -- 39
  -- (INST_RESUME, [opval])
  -- 40
  -- (INST_LANDINGPAD_OLD, [ty, val, val, num, id0, val0, ...])
  -- 41
  -- (INST_LOADATOMIC, [opty, op, align, vol, ordering, synchscope])
  -- 42
  -- (INST_STOREATOMIC_OLD, [ptrty, ptr, val, align, vol, odering, synchscope])
  -- 43
  (INST_GEP, (inbounds:opty:vs)) -> do
    oTy <- askType opty
    (val:idxs) <- mapM (getRelativeVal rs) vs
    return . Just $ I.Gep oTy (inbounds /= 0) val idxs
  -- 44
  (INST_STORE, [ ptr, val, align, vol ]) -> do
    ref <- getRelativeVal rs ptr
    val <- getRelativeVal rs val
    return . Just $ Store ref val (2^(align-1))
  -- 45
  -- (INST_STOREATOMIC, [ ptr, val, align, vol ])
  -- 46
  -- (INST_CMPXCHG, [ ptrty, ptr, valty, cmp, new, align, vol, ordering, synchscope])
  -- 47
  -- (INST_LANDINGPAD, [ ty, val, num, id0, val0, ...])
  -- 48
  -- (INST_CLEANUPRET, [val])
  -- (INST_CLEANUPRET, [val, bb#])
  -- 49
  -- (INST_CATCHRET, [val, bb#])
  -- 50
  -- (INST_CATCHPAD, [bb#, bb#, num, args...])
  -- 51
  -- (INST_CLEANUPPAD, [num, args...])
  -- 52
  -- (INST_CATCHSWITCH, [num, args...])
  -- (INST_CATCHSWITCH, [num, args..., bb])
  -- 53, 54 - Unused
  -- (OPERAND_BUNDLE, vals)
  -- ignore all other instructions for now.
  r -> fail $ show r

