#! /bin/sh

. tests/common.sh

sh scripts/make-all-previews.sh
$TEST_FS config set authorization no
$TEST_FS serve
