{ nixpkgs ? import <nixpkgs> {} }:
let
  overrides = pkgs: {
    haskell = pkgs.haskell // {
      packages = pkgs.haskell.packages // {
        ghcjs86 = pkgs.haskell.packages.ghcjs86.override {
          overrides = self: super: {
            mkDerivation = args: super.mkDerivation (args // { doCheck = false; });
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { config.packageOverrides = overrides; };

  project = import ./release.nix;
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  LANG = "en_US.UTF-8";
  buildInputs = 
  project.client.env.nativeBuildInputs ++ 
  project.server.env.nativeBuildInputs ++
    [
     pkgs.haskellPackages.cabal-install
     pkgs.git
     pkgs.entr
  ];
}
