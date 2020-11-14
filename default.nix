{ pkgs ? import ./dep/nixpkgs {} 
, useWarp ? false
, ... 
}:
let
  inherit (import ./dep/gitignore { inherit (pkgs) lib; }) gitignoreSource;
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
    root = gitignoreSource ./.;
    name = "ka";
    source-overrides = 
      let 
        cm = import ./dep/commonmark/thunk.nix; 
      in {
        reflex-fsnotify = import ./dep/reflex-fsnotify/thunk.nix;
        reflex-dom-pandoc = import ./dep/reflex-dom-pandoc/thunk.nix;
        commonmark = cm + "/commonmark";
        commonmark-extensions = cm + "/commonmark-extensions";
        commonmark-pandoc = cm + "/commonmark-pandoc";
      };
    overrides = self: super: with pkgs.haskell.lib; {
      reflex-dom = 
        if useWarp
          then addBuildDepend (enableCabalFlag super.reflex-dom "use-warp") self.jsaddle-warp
          else super.reflex-dom;
    };
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv extraDeps;
  }
