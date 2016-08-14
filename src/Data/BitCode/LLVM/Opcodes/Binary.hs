module Data.BitCode.LLVM.Opcodes.Binary where

-- | BinaryOpcodes - These are values used in the bitcode files to encode which
-- binop a CST_CODE_CE_BINOP or a XXX refers to.  The values of these enums
-- have no fixed relation to the LLVM IR enum values.  Changing these will
-- break compatibility with old files.
data BinOp
  = ADD -- 0
  | SUB -- 1
  | MUL -- 2
  | UDIV -- 3
  -- | overloaded for FP
  | SDIV -- 4
  | UREM -- 5
  -- | overloaded for FP
  | SREM -- 6
  | SHL -- 7
  | LSHR -- 8
  | ASHR -- 9
  | AND -- 10
  | OR -- 11
  | XOR -- 12
  deriving (Show, Enum, Eq, Ord)
