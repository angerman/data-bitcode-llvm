module Data.BitCode.LLVM.Codes.Type where

-- | = TYPE blocks have codes for each type primitive they use.
data Type
  = UNUSED0 -- 0
  -- | NUMENTRY: @[numentries]@
  | NUMENTRY -- 1
  -- | == Type Codes
  -- VOID
  | VOID -- 2
  -- | FLOAT
  | FLOAT -- 3
  -- | DOUBLE
  | DOUBLE -- 4
  -- | LABEL
  | LABEL -- 5
  -- | OPAQUE
  | OPAQUE -- 6
  -- | INTEGER: @[width]@
  | INTEGER -- 7
  -- | POINTER: @[pointee type, address space]@ -- address space is optional, defaults to 0.
  | POINTER -- 8
  -- | FUNCTION: @[vararg, attrid, retty, paramty x N]@
  | FUNCTION_OLD -- 9
  -- | HALF
  | HALF -- 10
  -- | ARRAY: [numelts, eltty]
  | ARRAY -- 11
  -- | VECTOR: [numelts, eltty]
  | VECTOR -- 12
  -- | These are not with the other floating point types because they're
  -- a late addition, and putting them in the right place breaks
  -- binary compatibility.
  --
  -- | X86 LONG DOUBLE
  | X86_FP80 -- 13
  -- | LONG DOUBLE (112 bit mantissa)
  | FP128 -- 14
  -- | PPC LONG DOUBLE (2 doubles)
  | PPC_FP128 -- 15
  -- | METADATA
  | METADATA -- 16
  -- | X86 MMX
  | X86_MMX -- 17
  -- | STRUCT_ANON: @[ispacked, eltty x N]@
  | STRUCT_ANON -- 18
  -- | STRUCT_NAME: @[strchr x N]@
  | STRUCT_NAME -- 19
  -- | STRUCT_NAMED: @[ispacked, eltty x N]@
  | STRUCT_NAMED -- 20
  -- | FUNCTION: @[vararg, retty, paramty x N]@
  | FUNCTION -- 21
  -- | TOKEN
  | TOKEN -- 22
  deriving (Show, Enum)
