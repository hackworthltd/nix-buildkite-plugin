import (builtins.fetchTarball {
  name = "nixpkgs-22.11";
  url = "https://github.com/nixos/nixpkgs/archive/4d2b37a84fad1091b9de401eb450aae66f1a741e.tar.gz";
  sha256 = "sha256:11w3wn2yjhaa5pv20gbfbirvjq6i3m7pqrq2msf0g7cv44vijwgw";
}) {
  overlays = import ./overlays.nix;
}
