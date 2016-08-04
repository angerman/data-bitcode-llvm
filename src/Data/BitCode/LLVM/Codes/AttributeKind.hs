module Data.BitCode.LLVM.Codes.AttributeKind where

data AttributeKind
  = ATTR_KIND_UNUSED0 -- 0
  | ATTR_KIND_ALIGNMENT -- 1
  | ATTR_KIND_ALWAYS_INLINE -- 2
  | ATTR_KIND_BY_VAL -- 3
  | ATTR_KIND_INLINE_HINT -- 4
  | ATTR_KIND_IN_REG -- 5
  | ATTR_KIND_MIN_SIZE -- 6
  | ATTR_KIND_NAKED -- 7
  | ATTR_KIND_NEST -- 8
  | ATTR_KIND_NO_ALIAS -- 9
  | ATTR_KIND_NO_BUILTIN -- 10
  | ATTR_KIND_NO_CAPTURE -- 11
  | ATTR_KIND_NO_DUPLICATE -- 12
  | ATTR_KIND_NO_IMPLICIT_FLOAT -- 13
  | ATTR_KIND_NO_INLINE -- 14
  | ATTR_KIND_NON_LAZY_BIND -- 15
  | ATTR_KIND_NO_RED_ZONE -- 16
  | ATTR_KIND_NO_RETURN -- 17
  | ATTR_KIND_NO_UNWIND -- 18
  | ATTR_KIND_OPTIMIZE_FOR_SIZE -- 19
  | ATTR_KIND_READ_NONE -- 20
  | ATTR_KIND_READ_ONLY -- 21
  | ATTR_KIND_RETURNED -- 22
  | ATTR_KIND_RETURNS_TWICE -- 23
  | ATTR_KIND_S_EXT -- 24
  | ATTR_KIND_STACK_ALIGNMENT -- 25
  | ATTR_KIND_STACK_PROTECT -- 26
  | ATTR_KIND_STACK_PROTECT_REQ -- 27
  | ATTR_KIND_STACK_PROTECT_STRONG -- 28
  | ATTR_KIND_STRUCT_RET -- 29
  | ATTR_KIND_SANITIZE_ADDRESS -- 30
  | ATTR_KIND_SANITIZE_THREAD -- 31
  | ATTR_KIND_SANITIZE_MEMORY -- 32
  | ATTR_KIND_UW_TABLE -- 33
  | ATTR_KIND_Z_EXT -- 34
  | ATTR_KIND_BUILTIN -- 35
  | ATTR_KIND_COLD -- 36
  | ATTR_KIND_OPTIMIZE_NONE -- 37
  | ATTR_KIND_IN_ALLOCA -- 38
  | ATTR_KIND_NON_NULL -- 39
  | ATTR_KIND_JUMP_TABLE -- 40
  | ATTR_KIND_DEREFERENCEABLE -- 41
  | ATTR_KIND_DEREFERENCEABLE_OR_NULL -- 42
  | ATTR_KIND_CONVERGENT -- 43
  | ATTR_KIND_SAFESTACK -- 44
  | ATTR_KIND_ARGMEMONLY -- 45
  | ATTR_KIND_SWIFT_SELF -- 46
  | ATTR_KIND_SWIFT_ERROR -- 47
  | ATTR_KIND_NO_RECURSE -- 48
  | ATTR_KIND_INACCESSIBLEMEM_ONLY -- 49
  | ATTR_KIND_INACCESSIBLEMEM_OR_ARGMEMONLY -- 50
  deriving (Show, Enum)
