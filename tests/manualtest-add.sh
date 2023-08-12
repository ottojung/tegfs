#! /bin/sh

. tests/common.sh

touch $TEGFS_ROOT/db/hi.txt
echo hi | $TEGFS add \
		--target hi.txt \
		--key a 1 \
		--key b 2 \
		--key SCHEDULED 3 \
