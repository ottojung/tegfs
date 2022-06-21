#! /bin/sh

tegfs query --format '%F "#tegfs-separator#" %P' %any | grep -v '//NA//' | sort --reverse | while IFS= read -r FILE
do
	TARGET="$(echo "$FILE" | awk -F '#tegfs-separator#' '{ print $1 }')"
	case "$TARGET" in
		 http://*) ;;
		 https://*) ;;
		 *) test -f "$TARGET" || continue ;;
	esac

	PREVIEW="$(echo "$FILE" | awk -F '#tegfs-separator#' '{ print $2 }')"
	test -f "$PREVIEW" && continue
	echo '>' tegfs make-thumbnails "$TARGET"
	tegfs make-thumbnails "$TARGET" "$PREVIEW" || touch "$PREVIEW"
done
