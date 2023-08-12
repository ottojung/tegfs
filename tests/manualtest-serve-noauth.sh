#! /bin/sh

. tests/common.sh

sh scripts/make-all-previews.sh $TEGFS
t_tegfs config set authorization no
t_tegfs serve
