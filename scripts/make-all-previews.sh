#! /bin/sh

tegfs list --dirs --format '%F "#tegfs-separator#" %P' | grep -v '//NA//' | while IFS= read -r FILE
do
	PREVIEW="$(echo "$FILE" | awk -F '#tegfs-separator#' '{ print $2 }')"
	test -f "$PREVIEW" && continue
	test -e "$TEGFS_ROOT/cache/failed-previews/$PREVIEW" && continue
	TARGET="$(echo "$FILE" | awk -F '#tegfs-separator#' '{ print $1 }')"
	echo '>' tegfs make-thumbnails "$TARGET"
	tegfs make-thumbnails "$TARGET" "$PREVIEW" || mkdir -p "$TEGFS_ROOT/cache/failed-previews/$PREVIEW"
done


