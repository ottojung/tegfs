#! /bin/sh

set -e
make test-files build
make test-config -B

TEST_FS=dist/tegfs
GUILE="guile -L src/ -s"
export TEGFS_ROOT=dist/exampleroot

touch $TEGFS_ROOT/db/hi.txt
echo hi | $TEST_FS add \
		--target hi.txt \
		--key a 1 \
		--key b 2 \
		--key SCHEDULED 3 \
