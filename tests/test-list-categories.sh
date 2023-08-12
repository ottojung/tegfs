#! /bin/sh

. tests/common.sh

RESULT=$($TEGFS --quiet categorization list-categories)
case "$RESULT" in
    "audio
video
image
text
song
recording
clip
lecture
photo
meme
drawing
selfie
book
pasta") ;;
    *)
        printf "Expected different entries, got:\n%s" "$RESULT" 1>&2
        exit 1
        ;;
esac
