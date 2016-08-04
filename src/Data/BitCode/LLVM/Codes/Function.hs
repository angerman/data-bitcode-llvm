module Data.BitCode.LLVM.Codes.Function where

-- | The function body block (FUNCTION_BLOCK_ID) describes function bodies.  It
-- can contain a constant block (CONSTANTS_BLOCK_ID).
data Instruction
  = INST_UNUSED0
  -- | DECLAREBLOCKS: [n]
  | DECLAREBLOCKS -- 1
  -- | BINOP:      [opcode, ty, opval, opval]
  | INST_BINOP -- 2
  -- | CAST:       [opcode, ty, opty, opval]
  | INST_CAST -- 3
  -- | GEP:        [n x operands]
  | INST_GEP_OLD -- 4
  -- | SELECT:     [ty, opval, opval, opval]
  | INST_SELECT -- 5
  -- | EXTRACTELT: [opty, opval, opval]
  | INST_EXTRACTELT -- 6
  -- | INSERTELT:  [ty, opval, opval, opval]
  | INST_INSERTELT -- 7
  -- | SHUFFLEVEC: [ty, opval, opval, opval]
  | INST_SHUFFLEVEC -- 8
  -- | CMP:        [opty, opval, opval, pred]
  | INST_CMP -- 9
  -- | RET:        [opty,opval<both optional>]
  | INST_RET -- 10
  -- | BR:         [bb#, bb#, cond] or [bb#]
  | INST_BR -- 11
  -- | SWITCH:     [opty, op0, op1, ...]
  | INST_SWITCH -- 12
  -- | INVOKE:     [attr, fnty, op0,op1, ...]
  | INST_INVOKE -- 13
  | INST_UNUSED14
  -- | UNREACHABLE
  | INST_UNREACHABLE -- 15
  -- | PHI:        [ty, val0,bb0, ...]
  | INST_PHI -- 16
  | INST_UNUSED17
  | INST_UNUSED18
  -- | ALLOCA:     [instty, opty, op, align]
  | INST_ALLOCA -- 19
  -- | LOAD:       [opty, op, align, vol]
  | INST_LOAD -- 20
  | INST_UNUSED21
  | INST_UNUSED22
  -- | VAARG:      [valistty, valist, instty]
  | INST_VAARG -- 23
  -- | This store code encodes the pointer type, rather than the value type
  -- this is so information only available in the pointer type (e.g. address
  -- spaces) is retained.
  -- | STORE:      [ptrty,ptr,val, align, vol]
  | INST_STORE_OLD -- 24
  | INST_UNUSED25
  -- | EXTRACTVAL: [n x operands]
  | INST_EXTRACTVAL -- 26
  -- | INSERTVAL:  [n x operands]
  | INST_INSERTVAL -- 27
  -- | fcmp/icmp returning Int1TY or vector of Int1Ty. Same as CMP, exists to
  -- support legacy vicmp/vfcmp instructions.
  -- | CMP2:       [opty, opval, opval, pred]
  | INST_CMP2 -- 28
  -- | new select on i1 or [N x i1]
  -- | VSELECT:    [ty,opval,opval,predty,pred]
  | INST_VSELECT -- 29
  -- | INBOUNDS_GEP: [n x operands]
  | INST_INBOUNDS_GEP_OLD -- 30
  -- | INDIRECTBR: [opty, op0, op1, ...]
  | INST_INDIRECTBR -- 31
  | INST_UNUSED32
  -- | DEBUG_LOC_AGAIN
  | DEBUG_LOC_AGAIN -- 33
  -- | CALL:    [paramattrs, cc[, fmf][, fnty], fnid, arg0, arg1...]
  -- fast math flag is set if the CallMarker FMF is present in the cc.
  -- fnty is set if the CallMaker ExplicitType is set.
  | INST_CALL -- 34
  -- | DEBUG_LOC:  [Line,Col,ScopeVal, IAVal]
  | DEBUG_LOC -- 35
  -- | FENCE: [ordering, synchscope]
  | INST_FENCE -- 36
  -- | CMPXCHG: [ptrty,ptr,cmp,new, align, vol, ordering, synchscope]
  | INST_CMPXCHG_OLD -- 37
  -- | ATOMICRMW: [ptrty,ptr,val, operation, align, vol,ordering, synchscope]
  | INST_ATOMICRMW -- 38
  -- | RESUME:     [opval]
  | INST_RESUME -- 39
  -- | LANDINGPAD: [ty,val,val,num,id0,val0...]
  | INST_LANDINGPAD_OLD -- 40
  -- | LOAD: [opty, op, align, vol,ordering, synchscope]
  | INST_LOADATOMIC -- 41
  -- | STORE: [ptrty,ptr,val, align, vol, ordering, synchscope]
  | INST_STOREATOMIC_OLD -- 42
  -- | GEP:  [inbounds, n x operands]
  | INST_GEP -- 43
  -- | STORE: [ptrty,ptr,valty,val, align, vol]
  | INST_STORE -- 44
  -- | STORE: [ptrty,ptr,val, align, vol
  | INST_STOREATOMIC -- 45
  -- | CMPXCHG: [ptrty,ptr,valty,cmp,new, align,vol,ordering,synchscope]
  | INST_CMPXCHG -- 46
  -- | LANDINGPAD: [ty,val,num,id0,val0...]
  | INST_LANDINGPAD -- 47
  -- | CLEANUPRET: [val] or [val,bb#]
  | INST_CLEANUPRET -- 48
  -- | CATCHRET: [val,bb#]
  | INST_CATCHRET -- 49
  -- | CATCHPAD: [bb#,bb#,num,args...]
  | INST_CATCHPAD -- 50
  -- | CLEANUPPAD: [num,args...]
  | INST_CLEANUPPAD -- 51
  -- | CATCHSWITCH: [num,args...] or [num,args...,bb]
  | INST_CATCHSWITCH -- 52
  | INST_UNUSED53
  | INST_UNUSED54
  -- | OPERAND_BUNDLE: [tag#, value...]
  | OPERAND_BUNDLE -- 55
  deriving (Show, Enum)
