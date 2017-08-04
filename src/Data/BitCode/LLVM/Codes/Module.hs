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
  -- | FUNCTION:  v1: [type, callingconv, isproto, linkage, paramattr, alignment, section, visibility, gc, unnamed_addr, prologuedata, dllstorageclass, comdat, prefixdata, personality]
  --              v2: [strtab_offset, strtab_size] ++ v1
  | FUNCTION -- 8,
  -- | ALIAS: [alias type, aliasee val#, linkage, visibility]
  | ALIAS_OLD --  9
  -- | PURGEVALS: [numvals]
  | PURGEVALS -- 10
  -- | GCNAME: [strchr x N]
  | GCNAME -- 11
  -- | COMDAT: v1: [selection_kind, name]
  --           v2: [strtab_offset, strtab_size, selection_kind]
  | COMDAT -- 12
  -- | VSTOFFSET: [offset]
  | VSTOFFSET --  13
  -- | ALIAS: [alias value type, addrspace, aliasee val#, linkage, visibility]
  | ALIAS --  14
  -- | METADATA_VALUES_UNUSED
  | METADATA_VALUES_UNUSED -- 15
  -- | SOURCE_FILENAME: [namechar x N]
  | SOURCE_FILENAME -- 16
  -- | HASH: [5*i32]
  | HASH -- 17
  -- | IFUNC: [ifunc value type, addrspace, resolver val#, linkage, visibility]
  | IFUNC -- 18
  deriving (Show, Enum)
