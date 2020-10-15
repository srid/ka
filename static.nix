# Sadly, this no longer works.
# FIXME: make it work. It did here[1].
# [1] https://github.com/srid/ka/tree/73caa31096589af66a1b97b772024c8b372c81e5

let 
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs-static { };
  pkgs = nixpkgs.pkgsMusl;
  ka = import ./default.nix { inherit pkgs; };
  inherit (pkgs.haskell.lib) appendConfigureFlags justStaticExecutables;
in 
  appendConfigureFlags (justStaticExecutables ka)
    [
      "--ghc-option=-optl=-static"
      "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
      "--extra-lib-dirs=${pkgs.zlib.static}/lib"
      "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
    ]
