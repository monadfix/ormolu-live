{ ormolu ? import ./nix/ormolu.nix
}:

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
        (self.callCabal2nix "ghc-lib-parser" ./ghc-lib-parser-8.10.1.20200412 { });
      ghc-syntax-highlighter = pkgs.haskell.lib.dontCheck
        (self.callHackageDirect {
           pkg = "ghc-syntax-highlighter";
           ver = "0.0.6.0";
           sha256 = "0891swy185smdycfzlmklki4q8l5xwqvav6zk3k1gdx968mg1hdr";
         } {});
      ormolu = pkgs.haskell.lib.dontCheck
        (self.callCabal2nix "ormolu" ormolu { });
      ormolu-live = optimize (pkgs.haskell.lib.dontCheck
        (self.callCabal2nix "ormolu-live" source { }));
    };
  };

in
  haskellPackages.ormolu-live
