module Data.BitCode.LLVM.Flags where

-- | OverflowingBinaryOperatorOptionalFlags - Flags for serializing
-- OverflowingBinaryOperator's SubclassOptionalData contents.
data OverflowingBinaryOperatorOptional
  = NO_UNSIGNED_WRAP -- 0
  | NO_SIGNED_WRAP -- 1
  deriving (Show, Enum, Eq)

-- | PossiblyExactOperatorOptionalFlags - Flags for serializing
-- PossiblyExactOperator's SubclassOptionalData contents.
data PossiblyExactOperatorOptional
  = EXACT -- 0
  deriving (Show, Enum, Eq)

-- | Markers and flags for call instruction.
data CallMarkers
  = CALL_TAIL -- 0
  | CALL_CCONV -- 1
  | CALL_UNUSED2 | CALL_UNUSED3 | CALL_UNUSED4 | CALL_UNUSED5  | CALL_UNUSED6
  | CALL_UNUSED7 | CALL_UNUSED8 | CALL_UNUSED9 | CALL_UNUSED10  | CALL_UNUSED11
  | CALL_UNUSED12 | CALL_UNUSED13
  | CALL_MUSTTAIL -- 14
  | CALL_EXPLICIT_TYPE -- 15
  | CALL_NOTAIL -- 16
  -- | Call has optional fast-math-flags.
  | CALL_FMF -- 17
  deriving (Enum, Show, Eq)

data Flag = Overflow OverflowingBinaryOperatorOptional
          | Exact PossiblyExactOperatorOptional
          | Call CallMarkers
          deriving (Show, Eq)

flagValue :: Flag -> Int
flagValue (Overflow x) = fromEnum x
flagValue (Exact x) = fromEnum x
flagValue (Call x) = fromEnum x
