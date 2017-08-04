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
import System.FilePath ((-<.>))
import Data.Either (isRight)

writeFile' :: FilePath -> [BitCode] -> IO ()
writeFile' fp = BCM.writeFile fp . withHeader True . emitTopLevel

compile :: FilePath -> IO FilePath
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

decompile :: FilePath -> IO FilePath
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

readBitcode :: FilePath -> IO (Either String (Maybe Ident, Module))
readBitcode f = do
  res <- BC.readFile f
  return $ (evalLLVMReader . parseTopLevel . catMaybes . map normalize) =<< res

moduleInstructions :: Module -> [I.Inst]
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

isModule :: Either String (Maybe Ident, Module) -> Bool
isModule = isRight


isCmpXchg :: I.Inst -> Bool
isCmpXchg (I.CmpXchg{}) = True
isCmpXchg _ = False

isFence :: I.Inst -> Bool
isFence (I.Fence{}) = True
isFence _ = False

isAtomicRMW :: I.Inst -> Bool
isAtomicRMW (I.AtomicRMW{}) = True
isAtomicRMW _ = False

isAtomicLoad :: I.Inst -> Bool
isAtomicLoad (I.AtomicLoad{}) = True
isAtomicLoad _ = False

isAtomicStore :: I.Inst -> Bool
isAtomicStore (I.AtomicStore{}) = True
isAtomicStore _ = False

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
      
--    xit "should be able to roundtrip FENCE"
    it "should be able to read ATOMIC RMW" $ do
      bcfile <- compile "test/fromBitcode/atomicrmw.ll"
      ret <- readBitcode bcfile
      ret `shouldSatisfy` isModule
      let Right (_mbIdent, mod) = ret
      moduleInstructions mod `shouldSatisfy` (any isAtomicRMW)
      
--    xit "should be able to roundtrip ATOMIC RMW"
    it "should be able to read LOAD ATOMIC" $ do
      bcfile <- compile "test/fromBitcode/atomicload.ll"
      ret <- readBitcode bcfile
      ret `shouldSatisfy` isModule
      let Right (_mbIdent, mod) = ret
      moduleInstructions mod `shouldSatisfy` (any isAtomicLoad)
      
--    xit "should be able to roundtrip LOAD ATOMIC"
    it "should be able to read STORE ATOMIC" $ do
      bcfile <- compile "test/fromBitcode/atomicstore.ll"
      ret <- readBitcode bcfile
      ret `shouldSatisfy` isModule
      let Right (_mbIdent, mod) = ret
      moduleInstructions mod `shouldSatisfy` (any isAtomicStore)

--    xit "should be able to roundtrip STORE ATOMIC"
