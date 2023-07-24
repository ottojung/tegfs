#! /bin/sh

. tests/common.sh

cp guix.scm dist/
$TEST_FS add --interactive --content dist/guix.scm
