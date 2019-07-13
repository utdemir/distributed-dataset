#!/usr/bin/env bash

set -o errexit
set -o pipefail

function trace() {
    echo "! $@" >&2; "$@"
}

function usage() {
cat << EOF
Usage:
  $0 format [--check]
  $0 dev <target> [ghcid-args...]
  
  $0 ci-build
  $0 nix-build
  $0 stack-build
  
  $0 help
EOF
}

function invalid_syntax() {
    echo "Invalid syntax." 2>&1
    usage 2>&1
    return 1
}

cd "$( dirname "${BASH_SOURCE[0]}" )"

[[ $# -lt 1 ]] && invalid_syntax

_orig_args="$*"
function ensure_nix_shell() {
    if [[ ! "$IN_NIX_SHELL" == "pure" ]]; then
        cmd="./make.sh $_orig_args"
        echo "Entering nix-shell..."
        nix-shell --pure --run "$cmd"
        exit $?
    fi
}

mode="$1"
shift

case "$mode" in
    "dev")
        [[ $# -lt 1 ]] && invalid_syntax
        ensure_nix_shell
        trace ghcid -c "cabal new-repl '$1'" $@
        ;;
    "format")
        [[ $# -gt 1 ]] && invalid_syntax
        if [[ $# == 0 ]]; 
        then mode="inplace"
        else
            ! [[ $1 == "--check" ]] && invalid_syntax
            mode="check"
        fi
        
        ensure_nix_shell
        trace find . -name '*.hs' \
            ! -path '*/dist-newstyle/*' \
            ! -path '*/.stack-work/*' \
            -execdir ormolu {} --mode "$mode" \;
        ;;
    "ci-build")
        [[ $# -gt 0 ]] && invalid_syntax
        trace ./make.sh format --check
        trace ./make.sh nix-build
        trace ./make.sh stack-build
        ;;
    "nix-build")
        [[ $# -gt 0 ]] && invalid_syntax
        # If I'm running it, also push the derivations to cachix.
        if [ "$USER" = "utdemir" ]; then 
          tmp="$(mktemp -d)"
          trap "rm -r '$tmp'" EXIT
          trace nix build -o $tmp/result
          echo $tmp/result | trace cachix push utdemir
        else 
          trace nix build --no-link
        fi
        ;;
    "stack-build")
        [[ $# -gt 0 ]] && invalid_syntax
        trace stack --no-nix test
        trace stack --stack-yaml nightly.yaml --no-nix test
        ;;
    "help")
        [[ $# -gt 0 ]] && invalid_syntax
        usage
        ;;
    *)
        invalid_syntax
esac

echo "Done."
