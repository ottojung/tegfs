#! /bin/sh

set -e
make test-files build
make test-config -B

TEST_FS=dist/tegfs
GUILE="guile --r7rs -L src/ -s"
export TEGFS_ROOT=dist/exampleroot

$TEST_FS get "non-existent-id"
$TEST_FS get "$(cat $TEGFS_ROOT/lastid.tegfs.txt)"
