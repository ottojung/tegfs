#! /bin/sh

set -e
make test-files build

TEST_FS=dist/tegfs
GUILE="guile -L src/ -s"
export TEGFS_ROOT=dist/exampleroot

sh scripts/make-all-previews.sh
$TEST_FS serve --no-authorization
