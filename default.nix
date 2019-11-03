let
  reflex-platform = import ./nix/reflex-platform.nix;

  pkgs = reflex-platform.nixpkgs;

  source = pkgs.lib.sourceByRegex ./. [
    "^ormolu-live\.cabal$"
    "^src.*$"
    "^LICENSE$"
  ];

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
        (self.callCabal2nix "ormolu" (pkgs.fetchFromGitHub {
           owner = "tweag";
           repo = "ormolu";
           sha256 = "0ai6a3957gzlkhkmnpvh3ngrk58rgc0wic5rysf1h2yddxlx6fd7";
           rev = "b08af17217e42e43042993b0ec4cbd3d00b158bb";
        }) { });
      ormolu-live = optimize (pkgs.haskell.lib.dontCheck
        (self.callCabal2nix "ormolu-live" source { }));
    };
  };

in
  haskellPackages.ormolu-live
