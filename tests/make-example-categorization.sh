#! /bin/sh

CATFILE="$TEST_ROOT/categorization.tegfs.txt"

echo "
		 audio video image text
		 audio : song recording
		 video : clip recording lecture
		 image : photo meme drawing
		 photo : selfie
		 text : book pasta
" > "$CATFILE"
