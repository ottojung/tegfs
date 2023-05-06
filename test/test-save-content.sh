#! /bin/sh

set -e
make test-files

TEST_FS=dist/tegfs
GUILE="guile -L src/ -s"
export TEGFS_ROOT=dist/exampleroot

cp guix.scm dist/
$TEST_FS save --content dist/guix.scm
