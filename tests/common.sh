#! /bin/sh

set -e
make --silent test-setup

TEST_FS=dist/tegfs
GUILE="guile --r7rs -L src/ -s"
export TEGFS_ROOT=dist/exampleroot-wd
