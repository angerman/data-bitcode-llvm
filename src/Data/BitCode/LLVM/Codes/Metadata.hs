module Data.BitCode.LLVM.Codes.Metadata where

data Metadata
  = METADATA_UNUSED0 -- 0
  -- | MDSTRING:      [values]
  | METADATA_STRING -- 1
  -- | VALUE:         [type num, value num]
  | METADATA_VALUE -- 2
  -- | NODE:          [n x md num]
  | METADATA_NODE -- 3
  -- | STRING:        [values]
  | METADATA_NAME -- 4
  -- | DISTINCT_NODE: [n x md num]
  | METADATA_DISTINCT_NODE -- 5
  -- | [n x [id, name]]
  | METADATA_KIND -- 6
  -- | [distinct, line, col, scope, inlined-at?]
  | METADATA_LOCATION -- 7
  -- | OLD_NODE:      [n x (type num, value num)]
  | METADATA_OLD_NODE -- 8
  -- | OLD_FN_NODE:   [n x (type num, value num)]
  | METADATA_OLD_FN_NODE -- 9
  -- | NAMED_NODE:    [n x mdnodes]
  | METADATA_NAMED_NODE -- 10
  -- | [m x [value, [n x [id, mdnode]]]
  | METADATA_ATTACHMENT -- 11
  -- | [distinct, tag, vers, header, n x md num]
  | METADATA_GENERIC_DEBUG -- 12
  -- | [distinct, count, lo]
  | METADATA_SUBRANGE -- 13
  -- | [distinct, value, name]
  | METADATA_ENUMERATOR -- 14
  -- | [distinct, tag, name, size, align, enc]
  | METADATA_BASIC_TYPE -- 15
  -- | [distinct, filename, directory]
  | METADATA_FILE -- 16
  -- | [distinct, ...]
  | METADATA_DERIVED_TYPE -- 17
  -- | [distinct, ...]
  | METADATA_COMPOSITE_TYPE -- 18
  -- | [distinct, flags, types]
  | METADATA_SUBROUTINE_TYPE -- 19
  -- | [distinct, ...]
  | METADATA_COMPILE_UNIT -- 20
  -- | [distinct, ...]
  | METADATA_SUBPROGRAM -- 21
  -- | [distinct, scope, file, line, column]
  | METADATA_LEXICAL_BLOCK -- 22
  -- | [distinct, scope, file, discriminator]
  | METADATA_LEXICAL_BLOCK_FILE -- 23
  -- | [distinct, scope, file, name, line]
  | METADATA_NAMESPACE -- 24
  -- | [distinct, scope, name, type, ...]
  | METADATA_TEMPLATE_TYPE -- 25
  -- | [distinct, scope, name, type, value, ...]
  | METADATA_TEMPLATE_VALUE -- 26
  -- | [distinct, ...]
  | METADATA_GLOBAL_VAR -- 27
  -- | [distinct, ...]
  | METADATA_LOCAL_VAR -- 28
  -- | [distinct, n x element]
  | METADATA_EXPRESSION -- 29
  -- | [distinct, name, file, line, ...]
  | METADATA_OBJC_PROPERTY -- 30
  -- | [distinct, tag, scope, entity, line, name]
  | METADATA_IMPORTED_ENTITY -- 31
  -- | [distinct, scope, name, ...]
  | METADATA_MODULE -- 32
  -- | [distinct, macinfo, line, name, value]
  | METADATA_MACRO -- 33
  -- | [distinct, macinfo, line, file, ...]
  | METADATA_MACRO_FILE -- 34
  deriving (Show, Enum)

