module Data.BitCode.LLVM.Codes.Module where

-- | MODULE blocks have a number of optional fields and subblocks.
data ModuleCode
  = SKIP -- 0
  -- | VERSION: [version#]
  | VERSION -- 1
  -- | TRIPLE: [strchr x N]
  | TRIPLE -- 2
  -- | DATALAYOUT: [strchr x N]
  | DATALAYOUT -- 3
  -- | ASM: [strchr x N]
  | ASM -- 4
  -- | SECTIONNAME: [strchr x N]
  | SECTIONNAME -- 5
  -- | DEPLIB: [strchr x N].  WARN: Will be removed in 4.0.
  | DEPLIB -- 6
  -- | GLOBALVAR: [pointer type, isconst, initid, linkage, alignment, section, visibility, threadlocal, unnamed_addr, externally_initialized, dllstorageclass, comdat]
  | GLOBALVAR -- 7
  -- | FUNCTION:  [type, callingconv, isproto, linkage, paramattr, alignment, section, visibility, gc, unnamed_addr, prologuedata, dllstorageclass, comdat, prefixdata, personality]
  | FUNCTION -- 8,
  -- | ALIAS: [alias type, aliasee val#, linkage, visibility]
  | ALIAS_OLD --  9
  -- | PURGEVALS: [numvals]
  | PURGEVALS -- 10
  -- | GCNAME: [strchr x N]
  | GCNAME -- 11
  -- | COMDAT: [selection_kind, name]
  | COMDAT -- 12
  -- | VSTOFFSET: [offset]
  | VSTOFFSET --  13
  -- | ALIAS: [alias value type, addrspace, aliasee val#, linkage, visibility]
  | ALIAS --  14
  -- | METADATA_VALUES: [numvals]
  | METADATA_VALUES -- 15
  deriving (Show, Enum)
