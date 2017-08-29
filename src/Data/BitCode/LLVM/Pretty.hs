{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RecordWildCards #-}
module Data.BitCode.LLVM.Pretty where

import Data.Word (Word64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.Char (toLower)
import Text.PrettyPrint
import Data.BitCode.LLVM
import Data.BitCode.LLVM.Value as V
import Data.BitCode.LLVM.Type  as T
import Data.BitCode.LLVM.Function as F
import Data.BitCode.LLVM.Instruction as I
import Data.BitCode.LLVM.CallingConv as C
import Data.BitCode.LLVM.Codes.AtomicOrdering (AtomicOrdering)
import Data.BitCode.LLVM.Codes.SynchronizationScope (AtomicSynchScope)
-- -----------------------------------------------------------------------------
-- Pretty class to simplify things a little
class Pretty a where
  pretty :: a -> Doc

instance (Pretty a) => Pretty [a] where
  pretty = hsep . punctuate comma . map pretty

instance (Pretty a, Pretty b) => Pretty [(a, b)] where
  pretty = vcat . map (\(x,y) -> pretty x <+> text "->" <+> pretty y)

instance Pretty Int where
  pretty = int

-- * Base types
instance {-# OVERLAPS #-} Pretty String where pretty = text
instance Pretty Word64 where pretty = text . show

instance (Pretty a) => Pretty (Maybe a) where
  pretty (Just x) = pretty x
  pretty Nothing  = empty

instance (Pretty a, Pretty b) => Pretty (Map a b) where
  pretty = pretty . Map.toList

prefix :: Value -> Doc
prefix (Global{}) = char '@'
prefix (V.Function{..}) | feProto fExtra  = text "decl "
                        | otherwise = text "def "
prefix (Alias{..}) = char '~'
prefix (Constant{}) = text "const "
prefix (Arg{}) = text "arg "
prefix (Value{}) = text "val "
prefix (TRef{}) = text "ref "
prefix (FwdRef i) = text "fwdRef" <+> int (fromIntegral i)

suffix :: Value -> Doc
suffix (Global{..}) = parens (pretty gInit) <+> text "::" <+> pretty gPointerType
suffix (V.Function{..}) = text "::" <+> pretty fType
suffix (Alias{..}) = empty
suffix (Constant t c) = pretty c <+> text "::" <+> pretty t
suffix (Arg t r) = int r <+> text "::" <+> pretty t
suffix (Value t) = text "::" <+> pretty t
suffix (TRef t r) = int r <+> text "::" <+> pretty t
suffix (FwdRef i) = empty

-- * Values
instance Pretty Value where
  pretty v@(V.Function{..}) | Just prefixData <- fePrefixData fExtra = prefix v <> suffix v $+$ text "Prefix:" <+> pretty prefixData
  pretty v = prefix v <> suffix v

-- * Symbols
instance Pretty Symbol where
  pretty (Unnamed _ _ v) = pretty v
  pretty (Named n _ _ v) = prefix v <> text n <+> suffix v
  pretty (Lazy n _ _) = text "lazy" <+> char '@' <> text n  

-- * Types
instance Pretty Ty where
  pretty Void = text "()"
  pretty T.Float = text "float"
  pretty Double = text "double"
  pretty (Opaque n) = text "opaque" <+> text n
  pretty (T.Int w) = char 'i' <> pretty w
  pretty (Ptr _ f@(T.Function{})) = parens (pretty f) <> char '*'
  pretty (Ptr _ t) = pretty t <> char '*'
  pretty (T.Array n t) = brackets $ pretty n <+> char 'x' <+> pretty t
  pretty (T.Vector n t) = char '<' <> pretty n <+> char 'x' <+> pretty t <> char '>'
  pretty (T.Function False retTy pTy) = parens (hsep (punctuate comma (map pretty pTy))) <+> text "->" <+> pretty retTy
  pretty (T.Function True retTy pTy) = parens (hsep (punctuate comma ((map pretty pTy) ++ [text "..."]))) <+> text "->" <+> pretty retTy
  -- use braces for structs.
  pretty (T.StructNamed n _ els) = text "struct" <+> text n <+> braces (hsep (punctuate comma (map pretty els)))
  pretty (T.StructAnon _ els) = text "struct" <+> braces (hsep (punctuate comma (map pretty els)))
  pretty x = error $ "Can not pretty print Ty: " ++ show x

-- * Constants
instance Pretty Const where
  pretty Null = text "null"
  pretty Undef = text "undef"
  pretty (V.Int n) = int n
  pretty (V.WideInt ns) = brackets $ hsep (map int ns)
  pretty (V.Float f) = text (show f)
  pretty (V.String s) = doubleQuotes $ text (escape s)
  pretty (V.CString s) = doubleQuotes $ text (escape s) <> text "\\0"
  pretty (V.InboundsGep t idxs) = parens $ text "getElemenentPointer inbounds" <+> parens (hsep . punctuate (text " !!") $ map pretty idxs) <+> text "::" <+> pretty t
  pretty (V.Struct vals) = text "struct" <+> braces (hsep (punctuate comma (map pretty vals)))
  pretty (V.Cast t op v) = text "cast" <+> text (map toLower (show op)) <+> pretty v <+> text "to" <+> pretty t
  pretty (V.BinOp op lhs rhs) = parens (pretty lhs) <+> text (map toLower $ show op) <+> parens (pretty rhs)
  pretty (V.Array vals) = brackets (hsep (punctuate comma (map pretty vals)))
  pretty x = error $ "Can not pretty print Const: " ++ show x

escape :: String -> String
escape [] = []
escape (h:t) | h == '\n' = '\\':'n':escape t
             | otherwise = h:escape t

prettyIndexed :: Pretty a => [a] -> [Doc]
prettyIndexed = map pretty' . zip [0..]
  where pretty' (n, p) = int n $$ nest 4 (colon <+> pretty p)

prettyWithIndex :: [Symbol] -> [Doc]
prettyWithIndex = map pretty'
  where pretty' :: Symbol -> Doc
        pretty' s@(Unnamed (Indexed _ i) _t _v) = int (fromIntegral (i 0)) $$ nest 4 (colon <+> pretty s)
        pretty' s@(Named _ (Indexed _ i) _t _v) = int (fromIntegral (i 0)) $$ nest 4 (colon <+> pretty s)
        pretty' s@(Lazy _ _ _)      = text "lazy" $$ nest 4 (colon <+> pretty s)

-- * Functions (Basic Blocks)
instance Pretty F.Function where
  pretty (F.Function{..}) = pretty dSig
    $$ text "Constants" <+> parens (int (length dConst)) $$ nest 3 (vcat (prettyIndexed dConst))
    $+$ text "Blocks" <+> parens (int (length dBody)) $$ nest 3 (vcat (prettyIndexed dBody))

instance Pretty BasicBlock where
  pretty (BasicBlock insts) = vcat (map pretty insts)

instance Pretty (Maybe Symbol, Inst) where
  pretty (Just (Unnamed _ _ (TRef t r)), inst) = text "ref" <+> int r <+> text "<-" <+> pretty inst
  pretty (Just (Named n _ _ (TRef t r)), inst) = text n <+> parens (text "ref" <+> int r) <+> text "<-" <+> pretty inst
  pretty (Nothing,  inst) = pretty inst

instance Pretty CallingConv where
  pretty c = text (map toLower (show c)) <> text "call"

instance Pretty TailCallKind where
  pretty None = empty
  pretty Tail = text "tail"
  pretty MustTail = text "must tail"
  pretty NoTail = text "no tail"

instance Pretty Inst where
  pretty (I.BinOp t o l r fs) = parens (pretty l) <+> text (map toLower $ show o) <+> vcat (map (text . map toLower . show) fs) <+> parens (pretty r) <+> text "::" <+> pretty t
  pretty (I.Cast t op v) = text "cast" <+> text (map toLower (show op)) <+> parens (pretty v) <+> text "::" <+> pretty t
  pretty (Alloca t v _) = text "alloca" <+> parens (pretty v) <+> text "::" <+> pretty t
  pretty (Load   t v _) = text "load" <+> parens (pretty v) <+> text "::" <+> pretty t
  pretty (Store  v r _) = pretty r <+> text "->" <+> pretty v
  pretty (Call   t tck cc s fty args)
    | f'@(V.Function{}) <- symbolValue s = pretty tck <+> pretty cc <+> fromMaybe (char 'f') (text <$> symbolName s) <> parens (hsep . punctuate comma $ map pretty args) <+> text "::" <+> pretty t
    | r'@(V.TRef{}) <- symbolValue s = pretty tck <+> pretty cc <+> pretty r' <> parens (hsep . punctuate comma $ map pretty args) <+> text "::" <+> pretty t
    | otherwise = text "WARN: Call without function or ref symbol not yet supported; are you sure you want this?"
  pretty (Cmp2   t l r p)  = parens (pretty l) <+> text (show p) <+> parens (pretty r) <+> text "::" <+> pretty t
  pretty (I.Gep  t ib v idxs) = text "getElementPointer" <+> (if ib then text "inbounds" else empty) <+> pretty v <+> text "!!" <+> (hsep . punctuate (text " !!") $ map pretty idxs)
  pretty (ExtractValue v idxs) = text "extract value" <+> pretty v <+> text "!!" <+> hcat (map (text . show) idxs) 
  pretty (Ret v)       = text "ret" <+> parens (pretty v)
  pretty (UBr bbId)    = text "br" <+> pretty bbId
  pretty (Br on bbId bbId') = text "br" <+> parens (pretty on) <+> pretty bbId <+> pretty bbId'
  pretty (Switch on defBbId cases) = text "case" <+> parens (pretty on) <+> text "of"
    $+$ nest 2 (vcat (map (\(val,bbId) -> pretty val <+> text "->" <+> pretty bbId) cases) $+$ text "_ ->" <+> pretty defBbId)
  pretty (Fence ord scope) = text "fence" <+> pretty ord <+> pretty scope
  pretty (CmpXchg dst cmp new ord scope ford) = text "cmpxchg"
  pretty (AtomicRMW dst val _op _ord _scope) = text "atomicRMW" <+> pretty dst <+> pretty val
  pretty (AtomicStore v r _ ord scope)   = text "atomic" <+> pretty r <+> text "->" <+> pretty v
  pretty (AtomicLoad t v _ ord scope)    = text "atomic load" <+> parens (pretty v) <+> text "::" <+> pretty t
-- -----------------------------------------------------------------------------
-- Identification
--
instance Pretty Ident where
  pretty (Ident s e) = text "Ident"
    $$ nest 3 (text s <+> int (fromEnum e))

-- -----------------------------------------------------------------------------
-- Atomic Ordering
--
instance Pretty AtomicOrdering where
  pretty = text . map toLower . drop 9 . show 

instance Pretty AtomicSynchScope where
  pretty = text . map toLower . show
-- -----------------------------------------------------------------------------
-- Module
--
instance Pretty Module where
  pretty (Module{..}) = text "Module"
    $$ nest 3 (
        text "Version"     $$ nest 12 (pretty mVersion)
    $+$ text "Triple"      $$ nest 12 (pretty mTriple)
    $+$ text "Datalayout"  $$ nest 12 (pretty mDatalayout)
    $+$ text "Constants"   <+> parens (int (length mConsts)) $$ nest 3 (vcat (prettyWithIndex mConsts))
    $+$ text "Globals"     <+> parens (int (length mValues)) $$ nest 3 (vcat (prettyWithIndex mValues))
    $+$ text "Fn Decls"    <+> parens (int (length mDecls))  $$ nest 3 (vcat (prettyWithIndex mDecls))
    $+$ text "Functions"   <+> parens (int (length mFns))    $$ nest 3 (vcat (prettyIndexed mFns))
    )
