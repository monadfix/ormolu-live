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
        (super.callHackage "ghc-lib-parser" "8.8.1" { });
      ghc-syntax-highlighter = pkgs.haskell.lib.dontCheck
        (self.callCabal2nix "ghc-syntax-highlighter" (pkgs.fetchFromGitHub {
           owner = "neongreen";
           repo = "ghc-syntax-highlighter";
           sha256 = "1d7d0diiqilw30d84cws8zqm36cc101qy2mxxbixbls5qd3kyk8a";
           rev = "8c47d4c04a72464fabf8fa55c6e7de6b54c22e8a";
        }) { });
      ormolu = pkgs.haskell.lib.dontCheck
        (self.callCabal2nix "ormolu" ormolu { });
      ormolu-live = optimize (pkgs.haskell.lib.dontCheck
        (self.callCabal2nix "ormolu-live" source { }));
    };
  };

in
  haskellPackages.ormolu-live
