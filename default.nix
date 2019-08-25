# This file is for nix (https://nixos.org/nix/) users only
# After install nix (curl https://nixos.org/nix/install | sh)
# you can run nix-shell --pure to give you a clean environment with only the dependencies listed below.
# This makes it easier to verify that the code has no external dependencies, 
# and to debug problems that might occur on only some machines, and not others.
# If your're not a nix-fan, just install gradle and java using the README.md, or look at the travis specification
with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "qwde-environment";

  buildInputs = with pkgs; [
    gradle
    openjdk12
    sqlite
  ];
}
