let 
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  extraDeps =
    if pkgs.lib.trivial.inNixShell
      then with pkgs.haskellPackages; [ 
          cabal-install
          ghcid 
          cabal-fmt 
          haskell-language-server 
          ormolu 
        ]
      else [];
in 
  pkgs.haskellPackages.developPackage {
    root = ./.;
    source-overrides = {
      reflex-fsnotify = sources.reflex-fsnotify;
    };
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv extraDeps;
  }
