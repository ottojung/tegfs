#! /bin/sh

. tests/common.sh

cp guix.scm dist/
$TEGFS add --interactive --content dist/guix.scm
