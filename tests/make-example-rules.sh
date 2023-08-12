#! /bin/sh

TARGET_ROOT="$1"
if test -z "$TARGET_ROOT"
then
    echo "Expected root directory as first argument" 1>&2
    exit 1
fi

mkdir -p "$TARGET_ROOT"

echo "
;; This file is for logical rules for tags

rule song video => clip
" > "$TARGET_ROOT/rules.tegfs.lisp"
