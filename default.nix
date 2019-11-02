let
  reflex-platform = import ./reflex-platform.nix;

  pkgs = reflex-platform.nixpkgs;

  ormolu-src = pkgs.fetchgit {
    url = "https://github.com/tweag/ormolu.git";
    sha256 = "0ai6a3957gzlkhkmnpvh3ngrk58rgc0wic5rysf1h2yddxlx6fd7";
    rev = "b08af17217e42e43042993b0ec4cbd3d00b158bb";
    fetchSubmodules = true;
  };

  haskellPackages = reflex-platform.ghcjs.override {
    overrides = self: super: {
      ghc-lib-parser = pkgs.haskell.lib.dontHaddock
        (super.callHackage "ghc-lib-parser" "8.8.1" { });
      ormolu = pkgs.haskell.lib.dontCheck
        (self.callCabal2nix "ormolu" "${ormolu-src}" { });
      ormolu-live = pkgs.haskell.lib.dontCheck
        (self.callCabal2nix "ormolu-live" ./pkg { });
      hpc = self.callHackage "hpc" "0.6.0.3" { };
    };
  };

  ormolu-live = haskellPackages.ormolu-live;

in
  {
    ormolu-live-release = pkgs.stdenv.mkDerivation rec {
      inherit (ormolu-live) name version;
      index-html = builtins.toFile "index.html" ''
        <html>
          <head>
            <script language="javascript" src="all.min.js" async></script>
          </head>
          <body></body>
        </html>
      '';
      buildCommand = ''
        mkdir -p $out
        cp ${index-html} $out/index.html
        ${pkgs.closurecompiler}/bin/closure-compiler \
          ${ormolu-live}/bin/ormolu-live.jsexe/all.js \
          --compilation_level=ADVANCED_OPTIMIZATIONS \
          --jscomp_off=checkVars \
          --externs=${ormolu-live}/bin/ormolu-live.jsexe/all.js.externs \
          > $out/all.min.js
      '';
    };

    shell = haskellPackages.shellFor {
      packages = ps: [
        ps.ormolu-live
      ];
      buildInputs = [
        (import (import ./nixpkgs.nix) { }).cabal-install
      ];
    };
  }

  # ormolu = pkgs.haskell.lib.overrideCabal haskellPackages.ormolu
  #   (drv: { buildFlags =
  #             (drv.buildFlags or []) ++
  #             ["--ghcjs-option=-O2 " "--ghcjs-option=-dedupe"];
  #         }
  #   );
