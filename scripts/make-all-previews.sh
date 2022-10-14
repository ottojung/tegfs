#! /bin/sh

tegfs query --dirpreview --format '%F "#tegfs-separator#" %P' '%any' | grep -v '//NA//' | while IFS= read -r FILE
do
	PREVIEW="$(echo "$FILE" | awk -F '#tegfs-separator#' '{ print $2 }')"
	test -f "$PREVIEW" && continue
	test -e "$TEGFS_ROOT/cache/failed-previews/$PREVIEW" && continue
	TARGET="$(echo "$FILE" | awk -F '#tegfs-separator#' '{ print $1 }')"
	echo "> $TARGET"
	tegfs make-thumbnails "$TARGET" "$PREVIEW" || mkdir -p "$TEGFS_ROOT/cache/failed-previews/$PREVIEW"
	echo
done

echo "All done!" 1>&2
