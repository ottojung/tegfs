#! /bin/sh

. tests/common.sh

sh scripts/make-all-previews.sh
$TEGFS serve
