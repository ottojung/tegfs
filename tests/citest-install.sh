#! /bin/sh

. tests/common.sh

sudo make --silent install
tegfs --version 1>/dev/null 2>/dev/null
tegfs --version 1>/dev/null
