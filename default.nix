let 
  src = import ./nix/sources.nix;
in 
{ pkgs ? import src.nixpkgs {} 
, useWarp ? false
, ... 
}:
let
  extraDeps =
    if pkgs.lib.trivial.inNixShell
      then with pkgs.haskellPackages; [ 
          cabal-install
          ghcid 
          cabal-fmt 
          # haskell-language-server  -- Let VSCode download latest
          ormolu 
        ]
      else [];
in 
  pkgs.haskellPackages.developPackage {
    root = ./.;
    name = "ka";
    source-overrides = {
      reflex-fsnotify = src.reflex-fsnotify;
      reflex-dom-pandoc = src.reflex-dom-pandoc;
      with-utf8 = src.haskell-with-utf8;
      algebraic-graphs = src.alga;
      commonmark = src.commonmark-hs + "/commonmark";
      commonmark-extensions = src.commonmark-hs + "/commonmark-extensions";
      commonmark-pandoc = src.commonmark-hs + "/commonmark-pandoc";
    };
    overrides = self: super: with pkgs.haskell.lib; {
      skylighting = super.skylighting_0_10_0_2;
      skylighting-core = super.skylighting-core_0_10_0_2;
      reflex-dom = 
        if useWarp
          then addBuildDepend (enableCabalFlag super.reflex-dom "use-warp") self.jsaddle-warp
          else super.reflex-dom;
    };
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv extraDeps;
  }