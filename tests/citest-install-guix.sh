#! /bin/sh

. tests/common.sh

if ! test "$CI_NIGHTLY" = 1
then exit 0
fi

if ! guix pull 2>guixlog
then
    cat guixlog 1>&2
    exit 1
fi

if ! guix package -f scripts/guix.scm 2>guixlog
then
    cat guixlog 1>&2
    exit 1
fi

if ! tegfs --version 1>/dev/null 2>guixlog
then
    cat guixlog 1>&2
    exit 1
fi

if ! guix remove tegfs 2>guixlog
then
    cat guixlog 1>&2
    exit 1
fi

rm -f guixlog