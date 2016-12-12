{ mkDerivation, base, binary, data-bitcode, pretty, stdenv }:
mkDerivation {
  pname = "data-bitcode-llvm";
  version = "3.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base binary data-bitcode pretty ];
  homepage = "https://github.com/angerman/data-llvm-bitcode#readme";
  description = "llvm bitcode reader and writer";
  license = stdenv.lib.licenses.bsd3;
}
