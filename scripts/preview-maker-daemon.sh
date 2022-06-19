#! /bin/sh

DIR="$( dirname -- "$( readlink -f "$0" )" )"

LASTFILE="$TEGFS_ROOT/lastid.tegfs.txt"
touch "$LASTFILE"

echo "$LASTFILE" | entr sh "$DIR/make-all-previews.sh"
