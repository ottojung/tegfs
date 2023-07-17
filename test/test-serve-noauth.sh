#! /bin/sh

set -e
make test-files build
make test-config -B

TEST_FS=dist/tegfs
GUILE="guile --r7rs -L src/ -s"
export TEGFS_ROOT=dist/exampleroot

sh scripts/make-all-previews.sh
$TEST_FS config set authorization no
$TEST_FS serve
