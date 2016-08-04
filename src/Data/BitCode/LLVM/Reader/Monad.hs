module Data.BitCode.LLVM.Reader.Monad
  ( LLVMReader
  , Result
  , evalLLVMReader
  , tellType,  askType
  , tellValue, askValue
  , ask, askTypeList, askValueList, tellValueList

  , tellValueSymbol, askValueSymbolTable, tellValueSymbolTable

  , tellMetadata, askMetadata, popMetadata
  , tellMetadataKind, askMetadataKind
  , askParamattr, tellParamattr
  , askParamattrGroup, tellParamattrGroup
  , tellIdent, askIdent

  , tellVersion, tellTriple, tellDataLayout

  , purgeValueList

  , nth
  )
  where

import Control.Exception (assert)
import Debug.Trace (traceStack)

import Control.Monad (MonadPlus(..))
import Control.Applicative (Alternative(..))
import Data.Maybe (fromMaybe)

-- | The LLVM IR essentially describes a @Module@
-- there can only be one.  Hence this reader mostly
-- deals with parsing a single module.

import Data.BitCode.LLVM -- essential data types
import Data.BitCode.LLVM.Type (Ty)
import Data.BitCode.LLVM.Value    (Value(FwdRef), Const, Symbol(..), ValueSymbolTable, ValueSymbolEntry, symbolValue, symbolName, entryName)
import Data.BitCode.LLVM.Function (Function)
import Data.BitCode.LLVM.Metadata (Metadata)
import Data.BitCode.LLVM.ParamAttr


type Result a = Either String a

-- | The module environemnt the reader
-- can access and modify.

data Ctx = Ctx
  {
  -- * Global Level
    identification :: Ident
  , version :: Int
  , triple :: String
  , dataLayout :: String
  , paramattrs :: [[GroupIdx]] -- ^ essentially the attributes are a merge of the groups.
  , paramattrGroups :: [(GroupIdx, ParamAttrGroupEntry)]
  -- * Module Level
  , types :: [Ty]
  , metadata :: [Metadata]
  , namedMetadata :: [(String, Metadata)]
  , metadataKinds :: [(Int, String)]
  , symbols :: [Symbol]
  , valueSymbolTable :: ValueSymbolTable
  } deriving Show

mkCtx :: Ctx
mkCtx = Ctx (error "Ident") (error "Version") (error "Triple") (error "Datalayout") mempty mempty mempty mempty mempty mempty mempty mempty

-- | A strict pair.
data PairS a = PairS { result :: !(Result a)
                     , ctx :: !Ctx
                     } deriving Show

newtype LLVMReader a = LLVM { runLLVMReader :: Ctx -> PairS a }

evalLLVMReader :: LLVMReader a -> Result a
evalLLVMReader = result . flip runLLVMReader mkCtx

-- * Functor
instance Functor LLVMReader where
  fmap f m = LLVM $ \c -> let PairS a c' = runLLVMReader m c
                           in PairS (f <$> a) c'

-- * Applicative
-- TODO: Verify `ap` is correct.  Instinctively the two
--       runLLVMReader calls should be independent, but
--       the second depends on the ctx of the evaluation
--       of the first.
instance Applicative LLVMReader where
  pure a = LLVM $ \c -> PairS (Right a) c

  m <*> n = LLVM $ \c ->
    let PairS f c' = runLLVMReader m c
        PairS x c'' = runLLVMReader n c'
    in PairS (f <*> x) c''

-- * Moand
instance Monad LLVMReader where
  m >>= n = LLVM $ \c -> case runLLVMReader m c of
    PairS (Right a) c' -> runLLVMReader (n a) c'
    PairS (Left err) c' -> PairS (Left err) c'

  fail s = LLVM $ \c -> PairS (Left s) c

-- * Monad Plus
instance MonadPlus LLVMReader where
  mzero = LLVM $ \c -> PairS (Left "") c
  m `mplus` n = LLVM $ \c -> let PairS a c' = runLLVMReader m c
                             in runLLVMReader n c'

instance Alternative LLVMReader where
  empty = mzero
  m <|> n = LLVM $ \c -> case runLLVMReader m c of
                           PairS (Left _) _ -> runLLVMReader n c
                           res              -> res

ask :: LLVMReader Ctx
ask = LLVM $ \c -> PairS (pure c) c

modify :: (Ctx -> Ctx) -> LLVMReader ()
modify f = LLVM $ \c -> PairS (pure ()) (f c)

nth :: (Integral a) => a -> [b] -> Maybe b
nth n xs | n < 0 = Nothing
         | otherwise = case take 1 (drop (fromIntegral n) xs) of
             [x] -> Just x
             []  -> Nothing

nth' :: (Integral a) => a -> [b] -> LLVMReader b
nth' n xs = assert (fromIntegral n < length xs) $ fromMaybe (error "Index out of range") . fmap pure . nth n $ xs

tellType :: Ty -> LLVMReader ()
tellType t = modify $ \c -> (c { types = types c ++ [t] })
askType :: (Integral a) => a -> LLVMReader Ty
askType n = nth' n =<< types <$> ask

symbolicate :: (Int, ValueSymbolEntry) -> [Symbol] -> [Symbol]
symbolicate (idx, entry) xs = case nth idx xs of
  Just s -> take idx xs ++ [Named (entryName entry) (symbolValue s)] ++ drop (idx+1) xs
  Nothing -> xs

-- | Adds a Value - Symbol element to the lookup table.
tellValueSymbol :: (Int, ValueSymbolEntry) -> LLVMReader ()
tellValueSymbol vs = modify $ \c -> c { valueSymbolTable = vs:valueSymbolTable c, symbols = symbolicate vs (symbols c) }

-- | Return the currently known VST
askValueSymbolTable :: LLVMReader ValueSymbolTable
askValueSymbolTable = valueSymbolTable <$> ask

-- | Set the VST.
-- Caveat, as values are resolved when the table is set or
-- the value is added, new symbols will only apply to these
-- values.  If you asked for a Value prior to setting the VST
-- that value will either have an invalid symbol or no symbol
-- at all.
--
-- TODO: Use some true Reference style solution or return
--       LLVMReader Value functions for Values that lookup
--       the symbol + value on call.
tellValueSymbolTable :: ValueSymbolTable -> LLVMReader ()
tellValueSymbolTable vst = modify $ \c -> c { valueSymbolTable = vst, symbols = foldr symbolicate (symbols c) vst }

tellValue :: Value -> LLVMReader ()
tellValue v = modify $ \c -> let
  n = length (symbols c)
  s = case lookup n (valueSymbolTable c) of
    Just e -> Named (entryName e) v
    Nothing -> Unnamed v
  in c { symbols = symbols c ++ [s] }

askValue :: (Integral a) => a -> LLVMReader Symbol
askValue n = fromMaybe (Unnamed (FwdRef (fromIntegral n))) . nth n . symbols <$> ask

tellMetadata :: Metadata -> LLVMReader ()
tellMetadata md = modify $ \c -> c { metadata = metadata c ++ [md] }
askMetadata :: (Integral a) => a -> LLVMReader Metadata
askMetadata n = LLVM $ \c -> PairS (pure $ metadata c !! (fromIntegral n)) c
popMetadata :: LLVMReader Metadata
popMetadata = LLVM $ \c -> let (h:t) = reverse (metadata c)
                           in PairS (pure h) (c { metadata = reverse t} )

tellMetadataKind :: (Int, String) -> LLVMReader ()
tellMetadataKind k = LLVM $ \c -> PairS (pure ()) (c { metadataKinds = k:metadataKinds c})

askMetadataKind :: (Integral a) => a -> LLVMReader String
askMetadataKind n = LLVM $ \c -> case lookup (fromIntegral n) (metadataKinds c) of
                                   Just v  -> PairS (pure v) c
                                   Nothing -> PairS (Left $ "No metadata for id: " ++ show (fromIntegral n)) c

tellParamattr :: [GroupIdx] -> LLVMReader ()
tellParamattr gidx = LLVM $ \c -> PairS (pure ()) (c { paramattrs = paramattrs c ++ [gidx] })
askParamattr :: (Integral a) => a -> LLVMReader [GroupIdx]
askParamattr n = (\c -> (paramattrs c !! (fromIntegral n))) <$> ask

tellParamattrGroup :: (GroupIdx, ParamAttrGroupEntry) -> LLVMReader ()
tellParamattrGroup g = LLVM $ \c -> PairS (pure ()) (c { paramattrGroups = g:paramattrGroups c })

askParamattrGroup :: GroupIdx -> LLVMReader ParamAttrGroupEntry
askParamattrGroup idx = LLVM $ \c -> case lookup idx (paramattrGroups c) of
                                       Just v  -> PairS (pure v) c
                                       Nothing -> PairS (Left $ "No paramattr group for idx: " ++ show idx) c

askTypeList :: LLVMReader [Ty]
askTypeList = types <$> ask

askValueList :: LLVMReader [Symbol]
askValueList = symbols <$> ask

tellIdent :: Ident -> LLVMReader ()
tellIdent i = modify $ \c -> c { identification = i }

askIdent :: LLVMReader Ident
askIdent = identification <$> ask

tellVersion :: (Integral a) => a -> LLVMReader ()
tellVersion v = modify $ \c -> c { version = fromIntegral v }

tellTriple :: String -> LLVMReader ()
tellTriple s = modify $ \c -> c { triple = s }

tellDataLayout :: String -> LLVMReader ()
tellDataLayout s = modify $ \c -> c { dataLayout = s }

purgeValueList :: (Integral a) => a -> LLVMReader ()
purgeValueList n = modify $ \c -> c { symbols = take (fromIntegral n) (symbols c) }

tellValueList :: [Symbol] -> LLVMReader ()
tellValueList ss = modify $ \c -> c { symbols = ss }
