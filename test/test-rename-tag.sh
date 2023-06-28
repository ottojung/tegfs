#! /bin/sh

set -e
make test-files build
make test-config -B

TEST_FS=dist/tegfs
GUILE="guile -L src/ -s"
export TEGFS_ROOT=dist/exampleroot

$GUILE example/rename-tag.scm
