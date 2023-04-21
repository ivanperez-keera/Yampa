let

myHaskellPackageOverlay = self: super: {
  myHaskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: rec {
      ihaskell-diagrams = hself.callCabal2nixWithOptions "ihaskell-diagrams" (builtins.fetchGit {
        url = "https://github.com/IHaskell/IHaskell";
        rev = "725d900414462da0f1859334a482e80c7a9e33d9";
      }) "--subpath ihaskell-display/ihaskell-diagrams" { };

    };
  };
};

in

{ nixpkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/refs/tags/22.11.tar.gz")
  {
    config.allowBroken = false;
    overlays = [ myHaskellPackageOverlay ];
  }
}:

# { nixpkgs ? import <nixpkgs> { overlays = [ myHaskellPackageOverlay ]; }, compiler ? "default", doBenchmark ? false }:

let

  pkgs = nixpkgs;

  haskellDeps = ps: with ps; [
    base deepseq diagrams diagrams-cairo ihaskell-diagrams plots random simple-affine-space
  ];

in

pkgs.stdenv.mkDerivation {
  name = "Whatever";

  buildInputs = [
    pkgs.libintlOrEmpty
    (pkgs.myHaskellPackages.ghcWithPackages haskellDeps)
  ];
}
