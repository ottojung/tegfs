#! /bin/sh

. tests/common.sh

# Adding tags "song" and "video"
#  but expecting to be queriable by "clip"
#  because of rules.tegfs.txt content

$TEST_FS --seed 777 add --tag song --tag video --content "hello this is a test content" 1>/dev/null

RESULT=$($TEST_FS --quiet query --format 'id "," mimetype "," tags' -- clip)
case "$RESULT" in
    "40k34vl0txfywxxjuqjq7kcrsqrga9,text/plain,audio song video") ;;
    *)
        printf "Expected different entries, got:\n%s" "$RESULT" 1>&2
        exit 1
        ;;
esac
