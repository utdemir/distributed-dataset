#!/usr/bin/env bash

set -o errexit
set -o errtrace

DIR="$(cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd)"

function err {
    echo "Test failed!"
    exit 1
}
trap err ERR

function trace() {
  echo "! $@" >&2; $@
}

# If I'm running it, also push the derivations to cachix.
if [ "$USER" = "utdemir" ]; then 
  function nix_build {
    tmp="$(mktemp -d)"
    trap "rm -r '$tmp'" EXIT
    trace nix build -o $tmp/result $@
    echo $tmp/result | trace cachix push utdemir
  }
else 
  function nix_build {
    trace nix build --no-link $@ 
  }
fi

nix_build
nix_build --argstr compiler ghc844
trace stack --silent --no-nix test 
trace stack --silent --no-nix test --stack-yaml nightly.yaml

echo "All good."
