#! /bin/sh

. tests/common.sh

touch t_tegfs_ROOT/db/hi.txt
echo hi | t_tegfs add \
		--target hi.txt \
		--key a 1 \
		--key b 2 \
		--key SCHEDULED 3 \
