#! /bin/sh

COUNTER=0

tegfs query --format '%F "#" %P' image | grep -v '//NA//' | sort --reverse | while IFS= read -r FILE
do
	TARGET="$(echo "$FILE" | awk -F '#' '{ print $1 }')"
	test -f "$TARGET" || continue
	PREVIEW="$(echo "$FILE" | awk -F '#' '{ print $2 }')"
	test -f "$PREVIEW" && continue
	echo '>' tegfs make-thumbnails "$TARGET"
	tegfs make-thumbnails "$TARGET" "$PREVIEW"
	COUNTER=$((COUNTER + 1))
done

echo "$COUNTER previews made" 1>&2
