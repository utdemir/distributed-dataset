#!/usr/bin/env sh

set -o xtrace
set -o errexit

docs="$(nix-build -A docs --no-out-link)"

cd "$(mktemp -d)"
git init
cp -r "$docs"/* .
git add .
git commit -m "Add docs"
git push --force git@github.com:utdemir/distributed-dataset master:gh-pages
