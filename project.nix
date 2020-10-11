{ system ? builtins.currentSystem
}:
let 
  gitignoreSrc = builtins.fetchTarball {
    url = "https://github.com/hercules-ci/gitignore/archive/c4662e6.tar.gz";
    sha256 = "1npnx0h6bd0d7ql93ka7azhj40zgjp815fw2r6smg8ch9p7mzdlx";
  };
  inherit (import (gitignoreSrc) { }) gitignoreSource;
  reflexPlatformSrc = builtins.fetchGit { 
    url = "https://github.com/reflex-frp/reflex-platform.git";
    # Note that reflex-platform is pinned to [PR #666] which is the release
    # condidate for the next version.
    # [PR #666]: https://github.com/reflex-frp/reflex-platform/pull/666
    ref = "rc/0.6.0.0";
    rev = "b14992ae74cac755f0e972569434c71ad35489eb";
  };
  reflexPlatform = import reflexPlatformSrc { 
    inherit system;
  };
  project = reflexPlatform.project ({pkgs, hackGet, ...}: {
    useWarp = true;
    withHoogle = false;
    packages = 
      let 
        srcs = {
          commonmark = hackGet ./dep/commonmark-hs;
          ob = hackGet ./dep/obelisk;
        };
      in {
        ka = pkgs.lib.cleanSource (gitignoreSource ./.);
        commonmark = srcs.commonmark + "/commonmark";
        commonmark-extensions = srcs.commonmark + "/commonmark-extensions";
        commonmark-pandoc = srcs.commonmark + "/commonmark-pandoc";
        reflex-dom-pandoc = hackGet ./dep/reflex-dom-pandoc;
        with-utf8 = hackGet ./dep/with-utf8;
        emojis = hackGet ./dep/emojis;
        algebraic-graphs = hackGet ./dep/alga;
        clay = hackGet ./dep/clay;
        pandoc-types = hackGet ./dep/pandoc-types;
        obelisk-route = srcs.ob + "/lib/route"; 
        obelisk-executable-config-lookup = srcs.ob + "/lib/executable-config/lookup";
        tabulation = srcs.ob + "/lib/tabulation";
      };
    overrides = self: super: with pkgs.haskell.lib; {
      algebraic-graphs = dontCheck super.algebraic-graphs; 

      skylighting = self.callHackageDirect {
        pkg = "skylighting";
        ver = "0.9";
        sha256 = "1zk8flzfafnmpb7wy7sf3q0biaqfh7svxz2da7wlc3am3n9fpxbr";
      } {};
      skylighting-core = self.callHackageDirect {
        pkg = "skylighting-core";
        ver = "0.9";
        sha256 = "1fb3j5kmfdycxwr7vjdg1hrdz6s61ckp489qj3899klk18pcmpnh";
      } {};
      relude = self.callHackageDirect {
        pkg = "relude";
        ver = "0.7.0.0";
        sha256 = "0flrwzxdd9bd3knk48zkhadwlad01msskjby1bfv4snr44q5xfqd";
      } {};
    };
    shells = {
      ghc = ["ka"];
      # This project doesn't use ghcjs, as it piggybacks on jsaddle
      # ghcjs = ["ka"];
    };
  });
in {
  inherit project reflexPlatform;
}
