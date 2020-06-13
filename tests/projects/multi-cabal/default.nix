{ compiler ? "ghc883" }:
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  hpkgs = pkgs.haskell.packages.ghc883;
  multi-cabal = hpkgs.developPackage {
    root = pkgs.nix-gitignore.gitignoreSource [] ./.;
    name = "multi-cabal";
    modifier = drv:
      with pkgs.haskellPackages;
      with pkgs.haskell.lib;
      disableOptimization (overrideCabal drv (attrs: {
        doCoverage = false;
        doCheck = true; # whether to run tests
        enableLibraryProfiling = false;
        enableExecutableProfiling = false;
        doHaddock = false;
        buildTools = [ brittany
                       cabal-install
                       ghcid
                       ghcide
                       hlint
                       hpack
                     ];
      }));
  };
in multi-cabal
