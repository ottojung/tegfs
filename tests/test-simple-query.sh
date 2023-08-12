#! /bin/sh

. tests/common.sh

case $(t_tegfs --quiet query --format mimetype -- image | sort | uniq) in
    "image/jpeg
inode/directory") ;;
    *)
        echo "Expected a different set of ids" 1>&2
        exit 1
        ;;
esac
