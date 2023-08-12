#! /bin/sh

mkdir -p "$TEGFS_ROOT"

echo "
audio video image text
audio : song recording
video : clip recording lecture
image : photo meme drawing
photo : selfie
text : book pasta
" > "$TEST_ROOT/categorization.tegfs.txt"
