There's two applications here, one `client` that takes Haskell code to javascript, and one `server` that writes the DOM logic. The `client` needs to be compiled with ghcjs, and the `server` needs to be compiled with ghc.

Somewhat hairy nix setup to get the environment done right, here's howto:

The idea:
`default.nix` says how to build the _derivation_. `release.nix` says what packages are used in the derivation. `shell.nix` is only for developers; it imports from `release.nix` to build an environment to do development in. Useful for cross-computer development.

`nix-build release.nix` builds the project. ./result/bin/server will execute the server  
`nix-shell --pure` lets you drop in to a developer env. You can now build server and client with `cabal build --ghc` and `cabal build --ghcjs`, respectively. REPL only works for server, which is then `cabal repl --ghc`.

### Build commands
I usually fire up a `screen` with three terminals.
```bash
# Terminal 1: build server on all changes
nix-shell release.nix --command "ag -l --no-color | grep -E \"shared|server\" | entr sh -c 'cabal build --ghc'"
# Terminal 2: build client on all changes
nix-shell release.nix --command "ag -l --no-color | grep -E \"shared|client\" | entr sh -c 'cabal build --ghc'"
# Terminal 3: compile and launch project
nix-build release.nix && ./result/bin/server
```
