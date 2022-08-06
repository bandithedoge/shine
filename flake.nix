{
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = {
    self,
    nixpkgs,
    flake-utils,
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
