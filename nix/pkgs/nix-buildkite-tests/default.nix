{ goldplate
, nix-buildkite
, runCommand
, src
}:
runCommand "nix-buildkite-tests"
{
  inherit src;
  nativeBuildInputs = [
    goldplate
    nix-buildkite
  ];
} ''
  set -e
  cd $src
  goldplate -j2 --pretty-diff .
  touch $out
''
