#! /bin/sh

. tests/common.sh

cp guix.scm dist/
$TEST_FS save --content dist/guix.scm
