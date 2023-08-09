#! /bin/sh

. tests/common.sh

$TEST_FS --seed 777 add --tag pasta --content "hello this is a test content" 1>/dev/null

RESULT=$($TEST_FS query --format 'id "," mimetype "," tags' -- pasta 2>/dev/null)
case "$RESULT" in
    "8shzfqitrslz9w7i7xppkp9gemuzba,text/plain,text pasta
40k34vl0txfywxxjuqjq7kcrsqrga9,text/plain,text pasta") ;;
    *)
        printf "Expected different entries, got:\n%s" "$RESULT" 1>&2
        exit 1
        ;;
esac
