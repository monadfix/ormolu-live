let
  pkgs = (import ./reflex-platform.nix).nixpkgs;

  ormolu-live = import ./default.nix;

in
  pkgs.stdenv.mkDerivation rec {
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
  }
