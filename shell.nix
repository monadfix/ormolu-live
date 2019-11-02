(import ./default.nix).env.overrideAttrs (attrs: {
  buildInputs = [
    (import (import ./nixpkgs.nix) { }).cabal-install
  ] ++ attrs.buildInputs;
})
