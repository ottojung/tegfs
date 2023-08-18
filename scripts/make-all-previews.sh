#! /bin/sh

TEGFS=tegfs
test -n "$1" && TEGFS="$1"

echo "Making previews..."
echo "Querying tegfs..."

FIRST=1

$TEGFS query --dirpreview --format '%F "#tegfs-separator#" %P' | grep -v '//NA//' | while IFS= read -r FILE
do
	if test $FIRST = 1
	then
		echo "Got resulting entries."
		FIRST=0
	fi

	PREVIEW="$(echo "$FILE" | awk -F '#tegfs-separator#' '{ print $2 }')"
	test -f "$PREVIEW" && continue
	test -e "$TEGFS_ROOT/cache/failed-previews/$PREVIEW" && continue
	TARGET="$(echo "$FILE" | awk -F '#tegfs-separator#' '{ print $1 }')"
	echo "> $TARGET"
	$TEGFS make-thumbnails "$TARGET" "$PREVIEW" || mkdir -p "$TEGFS_ROOT/cache/failed-previews/$PREVIEW"
	echo
done

echo "All done!" 1>&2
