#! /bin/sh

. tests/common.sh

$TEGFS get "non-existent-id"
$TEGFS get "$(cat $TEGFS_ROOT/lastid.tegfs.txt)"
