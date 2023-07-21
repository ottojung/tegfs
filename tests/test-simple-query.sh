#! /bin/sh

. tests/common.sh

case $($TEST_FS query --format mimetype -- image 2>/dev/null | sort | uniq) in
    "image/jpeg
inode/directory") ;;
    *)
        echo "Expected a different set of ids" 1>&2
        exit 1
        ;;
esac
