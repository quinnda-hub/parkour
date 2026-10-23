#!/bin/sh
# Downloads the JSON fixtures into bench/data/ (gitignored).
set -eu
dir="$(dirname "$0")/data"
base=https://raw.githubusercontent.com/miloyip/nativejson-benchmark/master/data
mkdir -p "$dir"
for f in canada twitter citm_catalog; do
  [ -s "$dir/$f.json" ] || curl -fsSL "$base/$f.json" -o "$dir/$f.json"
done
ls -l "$dir"
