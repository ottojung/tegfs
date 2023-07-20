#! /bin/sh

. tests/common.sh

$TEST_FS get "non-existent-id"
$TEST_FS get "$(cat $TEGFS_ROOT/lastid.tegfs.txt)"
