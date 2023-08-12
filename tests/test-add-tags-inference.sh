#! /bin/sh

. tests/common.sh

t_tegfs add --tag pasta --content "hello this is a test content" 1>/dev/null

RESULT=$(t_tegfs --quiet  query --format 'id "," mimetype "," tags' -- pasta)
case "$RESULT" in
    "10n2ldy9mb1fqzpn24lxnzxstibmds,text/plain,pasta text
40k34vl0txfywxxjuqjq7kcrsqrga9,text/plain,pasta text") ;;
    *)
        printf "Expected different entries, got:\n%s" "$RESULT" 1>&2
        exit 1
        ;;
esac
