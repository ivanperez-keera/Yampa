let

myHaskellPackageOverlay = self: super: {
  myHaskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: rec {
      Yampa = hself.callPackage  ../yampa { };
    };
  };
};

in

{ nixpkgs ? import <nixpkgs> { overlays = [ myHaskellPackageOverlay ]; }, compiler ? "default", doBenchmark ? false }:

let

  pkgs = nixpkgs;

  haskellDeps = ps: with ps; [
    base Cabal normaldistribution QuickCheck random tasty tasty-quickcheck Yampa
  ];

in

pkgs.stdenv.mkDerivation {
  name = "Whatever";

  buildInputs = [
    pkgs.libintlOrEmpty
    (pkgs.myHaskellPackages.ghcWithPackages haskellDeps)
  ];
}
