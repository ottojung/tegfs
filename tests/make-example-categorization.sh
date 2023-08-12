#! /bin/sh

TARGET_ROOT="$1"
if test -z "$TARGET_ROOT"
then
    echo "Expected root directory as first argument" 1>&2
    exit 1
fi

mkdir -p "$TARGET_ROOT"

echo "
audio video image text
audio : song recording
video : clip recording lecture
image : photo meme drawing
photo : selfie
text : book pasta
" > "$TARGET_ROOT/categorization.tegfs.txt"
