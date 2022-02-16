#!/bin/bash
set -eo pipefail


build_ghc_options="-j +RTS -A128m -n2m -N -RTS -fwarn-incomplete-patterns"


function if_then {
  if [[ -n "$1" ]]; then
    echo "$2"
  fi
}

function format {
  ormolu --mode inplace -ce \
  $(find . -name "*.hs" \
    -not -path "./*.stack-work/*" \
    -not -path "./.git/*")
}

function build_and_test {
  stack build \
  --ghc-options "$build_ghc_options" \
  --test \
  --fast
}

function build {
  stack build \
  --ghc-options "$build_ghc_options" \
  --fast
}

function bench {

  bench_work_dir=".bench.stack-work"
  pattern="$1"

  stack \
  --work-dir "$bench_work_dir" \
  build \
  --ghc-options "-O2 -rtsopts $build_ghc_options" \
  --bench \
  --ba "-s -m pattern \"$pattern\" +RTS -A128m -n2m -RTS"

}


format
bench
