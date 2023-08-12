#! /bin/sh

. tests/common.sh

EXAMPLEROOT=dist/exampleroot1

rm -rf "$EXAMPLEROOT"
sh tests/make-example-root.sh "$EXAMPLEROOT"
rm -rf "$EXAMPLEROOT/db"

rm -rf "tests/data-testroot"
mv -T -- "$EXAMPLEROOT" "tests/data-testroot"

git status --porcelain 1>/dev/null 2>/dev/null

if test -n "$(git status --porcelain)"
then
    echo "Recreated testroot is different from reference testroot." 1>&2
    git status --porcelain
    exit 1
fi
