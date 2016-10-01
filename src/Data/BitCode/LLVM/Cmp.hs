{-# LANGUAGE DeriveGeneric #-}
module Data.BitCode.LLVM.Cmp where

import GHC.Generics                      (Generic)
import Data.Binary                       (Binary)

-- | This enumeration lists the possible predicates for CmpInst subclasses.
-- Values in the range 0-31 are reserved for FCmpInst, while values in the
-- range 32-64 are reserved for ICmpInst. This is necessary to ensure the
-- predicate values are not overlapping between the classes.
data Predicate
  -- Opcode                   U L G E    Intuitive operation
  = FCMP_FALSE -- =  0,  ///< 0 0 0 0    Always false (always folded)
  | FCMP_OEQ   -- =  1,  ///< 0 0 0 1    True if ordered and equal
  | FCMP_OGT   -- =  2,  ///< 0 0 1 0    True if ordered and greater than
  | FCMP_OGE   -- =  3,  ///< 0 0 1 1    True if ordered and greater than or equal
  | FCMP_OLT   -- =  4,  ///< 0 1 0 0    True if ordered and less than
  | FCMP_OLE   -- =  5,  ///< 0 1 0 1    True if ordered and less than or equal
  | FCMP_ONE   -- =  6,  ///< 0 1 1 0    True if ordered and operands are unequal
  | FCMP_ORD   -- =  7,  ///< 0 1 1 1    True if ordered (no nans)
  | FCMP_UNO   -- =  8,  ///< 1 0 0 0    True if unordered: isnan(X) | isnan(Y)
  | FCMP_UEQ   -- =  9,  ///< 1 0 0 1    True if unordered or equal
  | FCMP_UGT   -- = 10,  ///< 1 0 1 0    True if unordered or greater than
  | FCMP_UGE   -- = 11,  ///< 1 0 1 1    True if unordered, greater than, or equal
  | FCMP_ULT   -- = 12,  ///< 1 1 0 0    True if unordered or less than
  | FCMP_ULE   -- = 13,  ///< 1 1 0 1    True if unordered, less than, or equal
  | FCMP_UNE   -- = 14,  ///< 1 1 1 0    True if unordered or not equal
  | FCMP_TRUE  -- = 15,  ///< 1 1 1 1    Always true (always folded)
  --  FIRST_FCMP_PREDICATE = FCMP_FALSE,
  --  LAST_FCMP_PREDICATE = FCMP_TRUE,
  --  BAD_FCMP_PREDICATE = FCMP_TRUE + 1,
  | SKIP_16 | SKIP_17 | SKIP_18 | SKIP_19 | SKIP_20 | SKIP_21 | SKIP_22 | SKIP_23
  | SKIP_24 | SKIP_25 | SKIP_26 | SKIP_27 | SKIP_28 | SKIP_29 | SKIP_30 | SKIP_31
  | ICMP_EQ   --  = 32,  ///< equal
  | ICMP_NE   --  = 33,  ///< not equal
  | ICMP_UGT  --  = 34,  ///< unsigned greater than
  | ICMP_UGE  --  = 35,  ///< unsigned greater or equal
  | ICMP_ULT  --  = 36,  ///< unsigned less than
  | ICMP_ULE  --  = 37,  ///< unsigned less or equal
  | ICMP_SGT  --  = 38,  ///< signed greater than
  | ICMP_SGE  -- = 39,  ///< signed greater or equal
  | ICMP_SLT  -- = 40,  ///< signed less than
  | ICMP_SLE  --  = 41,  ///< signed less or equal
  deriving (Eq, Enum, Show, Generic)
  --  FIRST_ICMP_PREDICATE = ICMP_EQ,
  --  LAST_ICMP_PREDICATE = ICMP_SLE,
  --  BAD_ICMP_PREDICATE = ICMP_SLE + 1

instance Binary Predicate
