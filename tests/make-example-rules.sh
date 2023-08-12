#! /bin/sh

mkdir -p "$TEGFS_ROOT"

echo "
;; This file is for logical rules for tags

rule song video => clip
" > "$TEGFS_ROOT/rules.tegfs.lisp"
