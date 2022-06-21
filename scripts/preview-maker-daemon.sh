#! /bin/sh

DIR="$( dirname -- "$( readlink -f "$0" )" )"

CACHEDIR="$TEGFS_ROOT/cache/preview"
LASTFILE="$TEGFS_ROOT/lastid.tegfs.txt"
touch "$LASTFILE"

find "$CACHEDIR" -size 0 -delete
echo "$LASTFILE" | entr sh "$DIR/make-all-previews.sh"
