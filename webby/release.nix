let
  jsaddle-src = pkgs.fetchFromGitHub {
    owner = "andsild";
    repo = "jsaddle";
    rev = "984e44c1ce3ca0f7e4b144f5e72b1393402f18aa";
    sha256 = "10kc8dfz04jz1i81iz3d4wng93s5sk5pbwvif7040nqsyn8dy8q3";
  };
  jsaddle-dom-src = pkgs.fetchFromGitHub {
    owner = "ghcjs";
    repo = "jsaddle-dom";
    rev = "da15a025eb06888cabf2e463b4f87ce14f65742b";
    sha256 = "1p4zbnqrizsrqk9h2vb576zk6wc8388qc040m1r8k1b48hv7vwmb";
  };
  ghcjs-dom-src = pkgs.fetchFromGitHub {
    owner = "ghcjs";
    repo = "ghcjs-dom";
    rev = "b8e483adef0cea66d081c1a014e87c6f99eb29fc";
    sha256 = "0xpsv1pp1a13vq5vk1wjj1iq0cfnq9cv7lkrv2rl6yd47slwmn2a";
  };

  miso = pkgs.haskell.packages.ghcjs86.callCabal2nix "miso" (pkgs.fetchFromGitHub {
    owner  = "dmjio";
    repo   = "miso";
    rev    = "f11b6a9eb8b69d71ac777975b13d3632d931f61e";
    sha256 = "1wl9vpqxshzrlanm9rpvgkiay3xa1abvkyknnp5z41kgfw63ypdl";
  }){};

  overrides = pkgs: {
    haskell = pkgs.haskell // {
      packages = pkgs.haskell.packages // {
        ghcjs86 = pkgs.haskell.packages.ghcjs86.override {
          overrides = self: super: with pkgs.haskell.lib; {
            mkDerivation = args: super.mkDerivation (args // {
              enableLibraryProfiling = false;
              doCheck = false;
              doHaddock = false;
            });
            jsaddle = self.callCabal2nix "jsaddle" "${jsaddle-src}/jsaddle" {};
            jsaddle-dom = self.callCabal2nix "jsaddle-dom" jsaddle-dom-src {};
            jsaddle-warp = dontCheck (self.callCabal2nix "jsaddle-warp" "${jsaddle-src}/jsaddle-warp" {});
            ghcjs-dom-jsaddle = self.callCabal2nix "ghcjs-dom-jsaddle" "${ghcjs-dom-src}/ghcjs-dom-jsaddle" {};
            ghcjs-dom-jsffi = self.callCabal2nix "ghcjs-dom-jsffi" "${ghcjs-dom-src}/ghcjs-dom-jsffi" {};
            ghcjs-dom = self.callCabal2nix "ghcjs-dom" "${ghcjs-dom-src}/ghcjs-dom" {};
            miso = miso;
          };
        };
        ghc865 = pkgs.haskell.packages.ghc865.override {
          overrides = self: super: with pkgs.haskell.lib; {
            mkDerivation = args: super.mkDerivation (args // {
              enableLibraryProfiling = false;
              doCheck = false;
              doHaddock = false;
            });
            ghcjs-dom-jsaddle = self.callCabal2nix "ghcjs-dom-jsaddle" "${ghcjs-dom-src}/ghcjs-dom-jsaddle" {};
            ghcjs-dom-jsffi = self.callCabal2nix "ghcjs-dom-jsffi" "${ghcjs-dom-src}/ghcjs-dom-jsffi" {};
            ghcjs-dom = self.callCabal2nix "ghcjs-dom" "${ghcjs-dom-src}/ghcjs-dom" {};
            jsaddle = self.callCabal2nix "jsaddle" "${jsaddle-src}/jsaddle" {};
            jsaddle-dom = self.callCabal2nix "jsaddle-dom" "${jsaddle-src}/jsaddle-dom" {};
            jsaddle-wkwebview = self.callCabal2nix "jsaddle-wkwebview" "${jsaddle-src}/jsaddle-wkwebview" {};
            jsaddle-warp = dontCheck (self.callCabal2nix "jsaddle-warp" "${jsaddle-src}/jsaddle-warp" {});
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> {
    config.packageOverrides = overrides;
    #config.allowBroken = true;
  };
in
  pkgs.haskellPackages.callPackage ./default.nix { }
