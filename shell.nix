(import ./default.nix { }).env.overrideAttrs (attrs: {
  buildInputs = [
    (import (import ./nix/nixpkgs.nix) { }).cabal-install
  ] ++ attrs.buildInputs;
})
