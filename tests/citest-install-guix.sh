#! /bin/sh

. tests/common.sh

if ! test "$CI_NIGHTLY" = 1
then exit 0
fi

guix pull

guix package -f scripts/guix.scm

tegfs --version

guix remove tegfs
