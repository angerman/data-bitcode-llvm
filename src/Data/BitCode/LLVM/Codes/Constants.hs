module Data.BitCode.LLVM.Codes.Constants where

-- | The constants block (CONSTANTS_BLOCK_ID) describes emission for each
-- constant and maintains an implicit current type value.
data Constant
  = CST_CODE_UNUSED0 -- 0
  -- | SETTYPE:       [typeid]
  | CST_CODE_SETTYPE -- 1
  -- | NULL
  | CST_CODE_NULL -- 2
  -- | UNDEF
  | CST_CODE_UNDEF -- 3
  -- | INTEGER:       [intval]
  | CST_CODE_INTEGER -- 4
  -- | WIDE_INTEGER:  [n x intval]
  | CST_CODE_WIDE_INTEGER -- 5
  -- | FLOAT:         [fpval]
  | CST_CODE_FLOAT -- 6
  -- | AGGREGATE:     [n x value number]
  | CST_CODE_AGGREGATE -- 7
  -- | STRING:        [values]
  | CST_CODE_STRING -- 8
  -- | CSTRING:       [values]
  | CST_CODE_CSTRING -- 9
  -- | CE_BINOP:      [opcode, opval, opval]
  | CST_CODE_CE_BINOP -- 10
  -- | CE_CAST:       [opcode, opty, opval]
  | CST_CODE_CE_CAST -- 11
  -- | CE_GEP:        [n x operands]
  | CST_CODE_CE_GEP -- 12
  -- | CE_SELECT:     [opval, opval, opval]
  | CST_CODE_CE_SELECT -- 13
  -- | CE_EXTRACTELT: [opty, opval, opval]
  | CST_CODE_CE_EXTRACTELT -- 14
  -- | CE_INSERTELT:  [opval, opval, opval]
  | CST_CODE_CE_INSERTELT -- 15
  -- | CE_SHUFFLEVEC: [opval, opval, opval]
  | CST_CODE_CE_SHUFFLEVEC -- 16
  -- | CE_CMP:        [opty, opval, opval, pred]
  | CST_CODE_CE_CMP -- 17
  -- | INLINEASM:     [sideeffect|alignstack,asmstr,conststr]
  | CST_CODE_INLINEASM_OLD -- 18
  -- | SHUFVEC_EX:    [opty, opval, opval, opval]
  | CST_CODE_CE_SHUFVEC_EX -- 19
  -- | INBOUNDS_GEP:  [n x operands]
  | CST_CODE_CE_INBOUNDS_GEP -- 20
  -- | CST_CODE_BLOCKADDRESS [fnty, fnval, bb#]
  | CST_CODE_BLOCKADDRESS -- 21
  -- | DATA:          [n x elements]
  | CST_CODE_DATA -- 22
  -- | INLINEASM:     [sideeffect|alignstack|asmdialect,asmstr,conststr]
  | CST_CODE_INLINEASM -- 23
  deriving (Show, Enum)
