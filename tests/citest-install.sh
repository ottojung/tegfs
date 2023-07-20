#! /bin/sh

. tests/common.sh

sudo make --silent install
tegfs --version 2>&1 | grep -v -e '^;;; ' | 1>/dev/null
