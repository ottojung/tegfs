#! /bin/sh

set -e
make test-files build

TEST_FS=dist/tegfs
GUILE="guile -L src/ -s"
export TEGFS_ROOT=dist/exampleroot

$TEST_FS get "non-existent-id"
$TEST_FS get "$(cat $TEGFS_ROOT/lastid.tegfs.txt)"
