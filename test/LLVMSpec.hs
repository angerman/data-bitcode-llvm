module LLVMSpec where

import Test.Tasty.Hspec

import Data.BitCode as BC
import Data.BitCode.Reader as BC
import Data.BitCode.LLVM.FromBitCode as BC
import Data.BitCode.LLVM as LLVM
import Data.BitCode.LLVM.Reader.Monad as LLVM
import qualified Data.BitCode.LLVM.Instruction as I
import qualified Data.BitCode.LLVM.Function    as F
import qualified Data.BitCode.Writer.Monad     as BCM (writeFile)
import Data.BitCode.Writer (emitTopLevel)
import Data.Maybe (catMaybes)
import Data.BitCode.LLVM.ToBitCode (toBitCode)
import Data.BitCode.Writer.Combinators (withHeader)

import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((-<.>), (</>))
import Data.Either (isRight)
import GHC.Stack (HasCallStack)

writeFile' :: HasCallStack => FilePath -> [BitCode] -> IO ()
writeFile' fp = BCM.writeFile fp . withHeader True . emitTopLevel

compile :: HasCallStack => FilePath -> IO FilePath
compile f = do
  (exit, _out, _err) <- readProcessWithExitCode
                        "clang"
                        [ "-w" -- no warnings
                        , "-emit-llvm"
                        , "-c"
                        , f
                        , "-o"
                        , fout ]
                        "" --stdin
  case exit of
    ExitSuccess -> return fout
    err         -> error $ show err
  where
    fout = f -<.> "bc"

decompile :: HasCallStack => FilePath -> IO FilePath
decompile f = do
  (exit, _out, _err) <- readProcessWithExitCode
                        "llvm-dis"
                        [ "-o"
                        , fout
                        , f ]
                        "" --stdin
  case exit of
    ExitSuccess -> return fout
    err         -> error $ show err
  where
    fout = f -<.> "dis"

readBitcode :: HasCallStack => FilePath -> IO (Either String (Maybe Ident, Module))
readBitcode f = do
  res <- BC.readFile f
  return $ (evalLLVMReader . parseTopLevel . catMaybes . map normalize) =<< res

moduleInstructions :: HasCallStack => Module -> [I.Inst]
moduleInstructions m =
  concatMap funcInsts (LLVM.mFns m)
  where
    funcInsts = concatMap blockInsts . F.dBody
    blockInsts :: F.BasicBlock -> [I.Inst]
    blockInsts (F.BasicBlock insts) = map snd insts
    blockInsts (F.NamedBlock _ insts) = map snd insts

-- Note: we often do not try to "write", as building
--       up modules by hand is rather hard. However
--       building modules is usually done with the
--       LLVM EDSL, and as such tests for writing modules
--       should be done there.

isModule :: HasCallStack => Either String (Maybe Ident, Module) -> Bool
isModule = isRight


isCmpXchg, isFence, isAtomicRMW, isAtomicLoad, isAtomicStore, isSwitch, isExtractValue
  :: HasCallStack => I.Inst -> Bool

isCmpXchg (I.CmpXchg{}) = True
isCmpXchg _ = False

isFence (I.Fence{}) = True
isFence _ = False

isAtomicRMW (I.AtomicRMW{}) = True
isAtomicRMW _ = False

isAtomicLoad (I.AtomicLoad{}) = True
isAtomicLoad _ = False

isAtomicStore (I.AtomicStore{}) = True
isAtomicStore _ = False

isSwitch (I.Switch{}) = True
isSwitch _ = False

isExtractValue (I.ExtractValue{}) = True
isExtractValue _ = False


compileModule :: HasCallStack => FilePath -> IO (FilePath, (Maybe Ident, Module))
compileModule fname = do
  bcfile <- compile $ "test/fromBitcode" </> fname
  ret <- readBitcode bcfile
  ret `shouldSatisfy` isModule
  let Right mod = ret
  return (bcfile, mod)

roundtripModule :: HasCallStack => FilePath -> IO [String]
roundtripModule fname = do
  (bcfile, mod) <- compileModule fname
  -- write the module back into the same file
  writeFile' bcfile . map denormalize $ toBitCode mod
  -- try to read it again
  ret <- readBitcode bcfile
  ret `shouldSatisfy` isModule
  -- make sure llvm doesn't throw up trying to decompile it
  decompile bcfile `shouldReturn` (bcfile -<.> "dis")
  lines <$> Prelude.readFile (bcfile -<.> "dis")

spec_llvm :: Spec
spec_llvm = do
  describe "fromBitcode" $ do
    it "should be able to read CMPXCHG" $ do
      bcfile <- compile "test/fromBitcode/cmpxchg.ll"
      ret <- readBitcode bcfile
      ret `shouldSatisfy` isModule
      let Right (_mbIdent, mod) = ret
      moduleInstructions mod `shouldSatisfy` (any isCmpXchg)

    it "should be able to roundtrip CMPXCHG" $ do
      bcfile <- compile "test/fromBitcode/cmpxchg.ll"
      ret <- readBitcode bcfile
      ret `shouldSatisfy` isModule
      let Right mod = ret
      writeFile' bcfile . map denormalize $ toBitCode mod
      ret <- readBitcode bcfile
      ret `shouldSatisfy` isModule
      decompile bcfile `shouldReturn` "test/fromBitcode/cmpxchg.dis"
 
    it "should be able to read FENCE" $ do
      bcfile <- compile "test/fromBitcode/fence.ll"
      ret <- readBitcode bcfile
      ret `shouldSatisfy` isModule
      let Right (_mbIdent, mod) = ret
      moduleInstructions mod `shouldSatisfy` (any isFence)

    it "should be able to roundtrip FENCE" $ do
      bcfile <- compile "test/fromBitcode/fence.ll"
      ret <- readBitcode bcfile
      ret `shouldSatisfy` isModule
      let Right mod = ret
      writeFile' bcfile . map denormalize $ toBitCode mod
      ret <- readBitcode bcfile
      ret `shouldSatisfy` isModule
      decompile bcfile `shouldReturn` "test/fromBitcode/fence.dis"
      
    it "should be able to read ATOMIC RMW" $ do
      bcfile <- compile "test/fromBitcode/atomicrmw.ll"
      ret <- readBitcode bcfile
      ret `shouldSatisfy` isModule
      let Right (_mbIdent, mod) = ret
      moduleInstructions mod `shouldSatisfy` (any isAtomicRMW)
      
    it "should be able to roundtrip ATOMIC RMW" $ do
      bcfile <- compile "test/fromBitcode/atomicrmw.ll"
      ret <- readBitcode bcfile
      ret `shouldSatisfy` isModule
      let Right mod = ret
      writeFile' bcfile . map denormalize $ toBitCode mod
      ret <- readBitcode bcfile
      ret `shouldSatisfy` isModule
      decompile bcfile `shouldReturn` "test/fromBitcode/atomicrmw.dis"
    
    it "should be able to read LOAD ATOMIC" $ do
      bcfile <- compile "test/fromBitcode/atomicload.ll"
      ret <- readBitcode bcfile
      ret `shouldSatisfy` isModule
      let Right (_mbIdent, mod) = ret
      moduleInstructions mod `shouldSatisfy` (any isAtomicLoad)
      
    it "should be able to roundtrip LOAD ATOMIC" $ do
      bcfile <- compile "test/fromBitcode/atomicload.ll"
      ret <- readBitcode bcfile
      ret `shouldSatisfy` isModule
      let Right mod = ret
      writeFile' bcfile . map denormalize $ toBitCode mod
      ret <- readBitcode bcfile
      ret `shouldSatisfy` isModule
      decompile bcfile `shouldReturn` "test/fromBitcode/atomicload.dis"
      
    it "should be able to read STORE ATOMIC" $ do
      bcfile <- compile "test/fromBitcode/atomicstore.ll"
      ret <- readBitcode bcfile
      ret `shouldSatisfy` isModule
      let Right (_mbIdent, mod) = ret
      moduleInstructions mod `shouldSatisfy` (any isAtomicStore)

    it "should be able to roundtrip STORE ATOMIC" $ do
      bcfile <- compile "test/fromBitcode/atomicstore.ll"
      ret <- readBitcode bcfile
      ret `shouldSatisfy` isModule
      let Right mod = ret
      writeFile' bcfile . map denormalize $ toBitCode mod
      ret <- readBitcode bcfile
      ret `shouldSatisfy` isModule
      decompile bcfile `shouldReturn` "test/fromBitcode/atomicstore.dis"

    it "should be able to read SWITCH" $ do
      bcfile <- compile "test/fromBitcode/switch.ll"
      ret <- readBitcode bcfile
      ret `shouldSatisfy` isModule
      let Right (_mbIdent, mod) = ret
      moduleInstructions mod `shouldSatisfy` (any isSwitch)

    it "should be able to roundtrip SWITCH" $ do
      bcfile <- compile "test/fromBitcode/switch.ll"
      ret <- readBitcode bcfile
      ret `shouldSatisfy` isModule
      let Right mod = ret
      writeFile' bcfile . map denormalize $ toBitCode mod
      ret <- readBitcode bcfile
      ret `shouldSatisfy` isModule
      decompile bcfile `shouldReturn` "test/fromBitcode/switch.dis"

    it "should be able to read EXTRACT VALUE" $ do
      (bcfile, (_mbIdent, mod)) <- compileModule "extractvalue.ll"
      moduleInstructions mod `shouldSatisfy` (any isExtractValue)

    it "should be able to roundtrip EXTRACT VALUE" $ do
      _ <- roundtripModule "extractvalue.ll"
      return ()
