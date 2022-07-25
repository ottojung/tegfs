#! /bin/sh

tegfs list --depth 2 --format '%F "#tegfs-separator#" %P' | grep -v '//NA//' | while IFS= read -r FILE
do
	PREVIEW="$(echo "$FILE" | awk -F '#tegfs-separator#' '{ print $2 }')"
	test -f "$PREVIEW" && continue
	test -e "$TEGFS_ROOT/cache/failed-previews/$PREVIEW" && continue
	TARGET="$(echo "$FILE" | awk -F '#tegfs-separator#' '{ print $1 }')"
	echo "THUMB $TARGET"
	tegfs make-thumbnails "$TARGET" "$PREVIEW" || mkdir -p "$TEGFS_ROOT/cache/failed-previews/$PREVIEW"
done

echo "All done!" 1>&2
