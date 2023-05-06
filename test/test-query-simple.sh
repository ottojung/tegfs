#! /bin/sh

set -e
make test-files

TEST_FS=dist/tegfs
GUILE="guile -L src/ -s"
export TEGFS_ROOT=dist/exampleroot

$TEST_FS query hi
