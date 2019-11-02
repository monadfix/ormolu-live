let
  reflex-platform = import ((import (import ./nixpkgs.nix) { }).fetchFromGitHub {
    owner = "reflex-frp";
    repo = "reflex-platform";
    rev = "8f4b8973a06f78c7aaf1a222f8f8443cd934569f";
    sha256  = "167smg7dyvg5yf1wn9bx6yxvazlk0qk64rzgm2kfzn9mx873s0vp";
  }) { };
in reflex-platform
