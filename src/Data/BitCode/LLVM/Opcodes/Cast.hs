module Data.BitCode.LLVM.Opcodes.Cast where

-- | CastOpcodes - These are values used in the bitcode files to encode which
-- cast a CST_CODE_CE_CAST or a XXX refers to.  The values of these enums
-- have no fixed relation to the LLVM IR enum values.  Changing these will
-- break compatibility with old files.
data CastOp
  = TRUNC -- 0
  | ZEXT -- 1
  | SEXT -- 2
  | FPTOUI -- 3
  | FPTOSI -- 4
  | UITOFP -- 5
  | SITOFP -- 6
  | FPTRUNC -- 7
  | FPEXT -- 8
  | PTRTOINT -- 9
  | INTTOPTR -- 10
  | BITCAST -- 11
  | ADDRSPACECAST -- 12
  deriving (Show, Enum, Eq)
