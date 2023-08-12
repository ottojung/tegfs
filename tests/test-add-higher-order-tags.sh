#! /bin/sh

. tests/common.sh

# Adding tags "song" and "video"
#  but expecting to be queriable by "clip"
#  because of rules.tegfs.txt content

t_tegfs add --tag person=X --tag song --content "hello this is a test content" 1>/dev/null

RESULT=$(t_tegfs --quiet query --format 'id "," mimetype "," tags' -- song)
case "$RESULT" in
    "8qka96sjlzzvnnrc6q23bua8bzxycv,audio/mpeg,song audio
ns1b5nlgc2wpoxrdv0605mcfsxmodz,audio/mpeg,song audio
40k34vl0txfywxxjuqjq7kcrsqrga9,text/plain,song person=X audio") ;;
    *)
        printf "Expected different entries, got:\n%s" "$RESULT" 1>&2
        exit 1
        ;;
esac
