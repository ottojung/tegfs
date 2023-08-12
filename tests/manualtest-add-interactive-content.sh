#! /bin/sh

. tests/common.sh

cp guix.scm dist/
t_tegfs add --interactive --content dist/guix.scm
