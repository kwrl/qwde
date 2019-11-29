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

  miso = pkgs.haskell.packages.ghcjs.callCabal2nix "miso" (pkgs.fetchFromGitHub {
    owner  = "dmjio";
    repo   = "miso";
    rev    = "ffb80294d14494fbb202a8e541111d3cc9a8a5b0";
    sha256 = "1h46mmlcszm4nmwq7yiy3sdks0xy3ww3pm41cz6zpg1hmikf40fm";
  }){};
in
  pkgs.haskellPackages.callPackage ./default.nix { inherit miso; }
