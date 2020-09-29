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
      reflex-dom-pandoc = sources.reflex-dom-pandoc;
    };
    overrides = self: super: {
      skylighting = super.skylighting_0_10_0_2;
      skylighting-core = super.skylighting-core_0_10_0_2;
    };
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv extraDeps;
  }
