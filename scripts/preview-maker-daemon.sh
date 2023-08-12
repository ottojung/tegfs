#! /bin/sh

DIR="$( dirname -- "$( readlink -f "$0" )" )"

CACHEDIR="$TEGFS_ROOT/cache/preview"
LASTFILE="$TEGFS_ROOT/lastid.tegfs.txt"
touch "$LASTFILE"

rm -rf "$TEGFS_ROOT/cache/failed-previews/$PREVIEW"
echo "$LASTFILE" | entr -n sh "$DIR/make-all-previews.sh" "$@"
