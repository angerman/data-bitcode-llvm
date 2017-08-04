module Data.BitCode.LLVM.IDs.Blocks where
-- | The only top-level block type defined is for a module.
data ModuleBlockID
  -- | Skip the first 8 blocks. To make the offset at FIRST_APPLICATION_BLOCKID (8).
  = SKIP_0 | SKIP_1 | SKIP_2 | SKIP_3 | SKIP_4 | SKIP_5 | SKIP_6 | SKIP_7
  -- | Blocks
  | MODULE -- FIRST_APPLICATION_BLOCKID (8)
  -- | Module sub-block id's.
  | PARAMATTR -- 9
  | PARAMATTR_GROUP -- 10
  | CONSTANTS -- 11
  | FUNCTION -- 12
  -- | Block intended to contains information on the bitcode versioning.
  -- Can be used to provide better error messages when we fail to parse a
  -- bitcode file.
  | IDENTIFICATION -- 13
  | VALUE_SYMTAB -- 14
  | METADATA -- 15
  | METADATA_ATTACHMENT_ID -- 16
  | TYPE_NEW -- 17
  | USELIST -- 18
  | MODULE_STRTAB -- 19
  | FUNCTION_SUMMARY -- 20
  | OPERAND_BUNDLE_TAGS -- 21
  | METADATA_KIND -- 22
  | STRTAB -- 23
  | FULL_LTO_GLOBAL_SUMMARY -- 24
  | SYMTAB -- 24
  | SYNC_SCOPE_NAMES -- 25
  deriving (Show, Enum)

-- x = ( Ident {iString = "APPLE_1_703.0.31_0", iEpoch = Current}
--     , Module
--       { mVersion = 1
--       , mTriple = Just "x86_64-apple-macosx10.11.0"
--       , mDatalayout = Just "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
--       , mValues = [ Global {gPointerType = Array {teNumElts = 13, teEltTy = Int {teWidth = 8}}, gIsConst = True, gAddressSpace = 0, gInitId = Just 3, gLinkage = ExternalWeak, gParamAttrs = 1, gSection = 0, gVisibility = Default, gThreadLocal = NotThreadLocal, gUnnamedAddr = True, gExternallyInitialized = False, gDLLStorageClass = Default, gComdat = 0}
--                   , Function {fType = Function {teVarArg = False, teRetTy = Int {teWidth = 32}, teParamTy = [Int {teWidth = 32},Ptr {teAddressSpace = 0, tePointeeTy = Ptr {teAddressSpace = 0, tePointeeTy = Int {teWidth = 8}}}]}, fCallingConv = C, fIsProto = False, fLinkage = External, fParamAttrs = 1, fAlignment = 0, fSection = 0, fVisibility = Default, fGC = 0, fUnnamedAddr = False, fPrologueData = 0, fDLLStorageClass = Default, fComdat = 0, fPrefixData = 0, fPersonalityFn = 0}
--                   , Function {fType = Function {teVarArg = True, teRetTy = Int {teWidth = 32}, teParamTy = [Ptr {teAddressSpace = 0, tePointeeTy = Int {teWidth = 8}}]}, fCallingConv = C, fIsProto = True, fLinkage = External, fParamAttrs = 2, fAlignment = 0, fSection = 0, fVisibility = Default, fGC = 0, fUnnamedAddr = False, fPrologueData = 0, fDLLStorageClass = Default, fComdat = 0, fPrefixData = 0, fPersonalityFn = 0}
--                   , Constant (Array {teNumElts = 13, teEltTy = Int {teWidth = 8}}) (CString "hello world\n")
--                   , Constant (Int {teWidth = 32}) (Int 2)
--                   , Constant (Int {teWidth = 32}) (Int 4)
--                   ]
--       , mDecls = [ Function {fType = Function {teVarArg = True, teRetTy = Int {teWidth = 32}, teParamTy = [Ptr {teAddressSpace = 0, tePointeeTy = Int {teWidth = 8}}]}, fCallingConv = C, fIsProto = True, fLinkage = External, fParamAttrs = 2, fAlignment = 0, fSection = 0, fVisibility = Default, fGC = 0, fUnnamedAddr = False, fPrologueData = 0, fDLLStorageClass = Default, fComdat = 0, fPrefixData = 0, fPersonalityFn = 0} ]
--       , mFns = [ Function
--                  { dSig = Function {fType = Function {teVarArg = False, teRetTy = Int {teWidth = 32}, teParamTy = [Int {teWidth = 32},Ptr {teAddressSpace = 0, tePointeeTy = Ptr {teAddressSpace = 0, tePointeeTy = Int {teWidth = 8}}}]}, fCallingConv = C, fIsProto = False, fLinkage = External, fParamAttrs = 1, fAlignment = 0, fSection = 0, fVisibility = Default, fGC = 0, fUnnamedAddr = False, fPrologueData = 0, fDLLStorageClass = Default, fComdat = 0, fPrefixData = 0, fPersonalityFn = 0}
--                  , dConst = [ Constant (Int {teWidth = 32}) Null
--                             , Constant (Ptr {teAddressSpace = 0, tePointeeTy = Int {teWidth = 8}}) (InboundsGep [1,2,0,3,8,3,8])
--                             ]
--                  , dBody = BasicBlock [ Alloca (Int {teWidth = 32}) (Constant (Int {teWidth = 32}) (Int 2)) 0
--                                       , Alloca (Ptr {teAddressSpace = 0, tePointeeTy = Ptr {teAddressSpace = 0, tePointeeTy = Int {teWidth = 8}}}) (Constant (Int {teWidth = 32}) (Int 2)) 0
--                                       , Store (TRef (Int {teWidth = 32}) 0) (Arg (Int {teWidth = 32})) 4
--                                       , Store (TRef (Ptr {teAddressSpace = 0, tePointeeTy = Ptr {teAddressSpace = 0, tePointeeTy = Int {teWidth = 8}}}) 1) (Arg (Ptr {teAddressSpace = 0, tePointeeTy = Ptr {teAddressSpace = 0, tePointeeTy = Int {teWidth = 8}}})) 8
--                                       , Call (Function {teVarArg = True, teRetTy = Int {teWidth = 32}, teParamTy = [Ptr {teAddressSpace = 0, tePointeeTy = Int {teWidth = 8}}]}) (Function {fType = Function {teVarArg = True, teRetTy = Int {teWidth = 32}, teParamTy = [Ptr {teAddressSpace = 0, tePointeeTy = Int {teWidth = 8}}]}, fCallingConv = C, fIsProto = True, fLinkage = External, fParamAttrs = 2, fAlignment = 0, fSection = 0, fVisibility = Default, fGC = 0, fUnnamedAddr = False, fPrologueData = 0, fDLLStorageClass = Default, fComdat = 0, fPrefixData = 0, fPersonalityFn = 0}) [Constant (Array {teNumElts = 13, teEltTy = Int {teWidth = 8}}) (CString "hello world\n")]
--                                       , Ret (Just (Constant (Int {teWidth = 32}) Null))]
--                  }
--                ]
--       }
--     )
