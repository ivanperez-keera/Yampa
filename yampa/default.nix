{ mkDerivation, base, deepseq, lib, random, simple-affine-space }:
mkDerivation {
  pname = "Yampa";
  version = "0.14.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base deepseq random simple-affine-space
  ];
  homepage = "https://github.com/ivanperez-keera/Yampa/";
  description = "Elegant Functional Reactive Programming Language for Hybrid Systems";
  license = lib.licenses.bsd3;
}
