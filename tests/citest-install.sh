#! /bin/sh

. tests/common.sh

if command -v sudo 1>/dev/null 2>/dev/null
then MAYBESUDO=sudo
else MAYBESUDO=""
fi

$MAYBESUDO make --silent install
tegfs --version 1>/dev/null
