#! /bin/sh

. tests/common.sh

cd docs
make --silent html 1>/dev/null
