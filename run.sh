#!/usr/bin/env bash
set -euo pipefail
stack run "$1" | gcc -x assembler -
set +e
./a.out
echo $?
