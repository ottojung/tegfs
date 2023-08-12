#! /bin/sh

. tests/common.sh

t_tegfs get "non-existent-id"
t_tegfs get "$(cat t_tegfs_ROOT/lastid.tegfs.txt)"
