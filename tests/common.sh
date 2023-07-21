#! /bin/sh

set -e
make --silent test-files build
make --silent test-config -B

TEST_FS=dist/tegfs
GUILE="guile --r7rs -L src/ -s"
export TEGFS_ROOT=dist/exampleroot