{
  nixConfig = {
    substituters = ["https://shine.cachix.org" "https://cache.iog.io"];
    trusted-public-keys = ["shine.cachix.org-1:nCuHaF+O1/gg0pwwx3z4pIOyqKpAZ8zxwCbSDzgYwh0=" "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
  };
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
  };
  outputs = {
    self,
    nixpkgs,
    flake-utils,
    flake-compat,
    haskellNix,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      overlays = [
        haskellNix.overlay
        (final: prev: {
          shine = final.haskell-nix.project' {
            name = "shine";
            src = ./.;
            compiler-nix-name = "ghc902";
            shell.tools = {
              cabal = {};
              haskell-language-server = {};
            };
          };
        })
      ];
      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };
      flake = pkgs.shine.flake {};
    in
      flake
      // {
        defaultPackage = flake.packages."shine:exe:shine";
      });
}
