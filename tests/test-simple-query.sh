#! /bin/sh

. tests/common.sh

RESULT=$(t_tegfs --quiet query --format mimetype -- image | sort | uniq)

case "$RESULT" in
    "image/jpeg
inode/directory
tegfs/entry") ;;
    *)
        printf "Expected a different set of mimetypes, got:\n%s" "$RESULT" 1>&2
        exit 1
        ;;
esac
