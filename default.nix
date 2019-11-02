let
  reflex-platform = import ./reflex-platform.nix;

  pkgs = reflex-platform.nixpkgs;

  ormolu-src = pkgs.fetchgit {
    url = "https://github.com/tweag/ormolu.git";
    sha256 = "0ai6a3957gzlkhkmnpvh3ngrk58rgc0wic5rysf1h2yddxlx6fd7";
    rev = "b08af17217e42e43042993b0ec4cbd3d00b158bb";
    fetchSubmodules = true;
  };

  optimize = pkg: pkgs.haskell.lib.overrideCabal pkg (drv: {
    buildFlags =
      (drv.buildFlags or []) ++ [
        "--ghcjs-option=-DGHCJS_BROWSER"
        "--ghcjs-option=-O2"
        "--ghcjs-option=-dedupe"
      ];
  });

  haskellPackages = reflex-platform.ghcjs.override {
    overrides = self: super: {
      hpc = self.callHackage "hpc" "0.6.0.3" { };
      ghc-lib-parser = pkgs.haskell.lib.dontHaddock
        (super.callHackage "ghc-lib-parser" "8.8.1" { });
      ormolu = pkgs.haskell.lib.dontCheck
        (self.callCabal2nix "ormolu" "${ormolu-src}" { });
      ormolu-live = optimize (pkgs.haskell.lib.dontCheck
        (self.callCabal2nix "ormolu-live" ./pkg { }));
    };
  };

in
  haskellPackages.ormolu-live
