#! /bin/sh

set -e

FILES=$(ls tests/test-* || true)

case "$CI" in
    1)
        FILES="$FILES
$(ls tests/citest-* || true)"
        ;;
esac

TESTCOUNT=$(echo "$FILES" | wc -l)
INDEX=0
FAILED=0

for FILE in $FILES
do
	INDEX=$((INDEX + 1))
	SHORT="$(basename "$FILE")"

	printf '(%s/%s)' "$INDEX" "$TESTCOUNT"
	printf ' %s ... ' "$SHORT"

	case "$FILE" in
		*.sld)
			R=$(if guile --r7rs -L "src" -L "tests" -s "$FILE" 2>&1
			    then printf '✓'
			    else printf 'X'
			    fi)
			;;
		*.sh)
			R=$(if sh "$FILE" 2>&1
			    then printf '✓'
			    else printf 'X'
			    fi)
			;;
	esac

	printf '%s' "$R" | grep -v -e '^;;; ' || true
	printf '%s' "$R" | grep -q -e '✓' || FAILED=1
done

if test "$FAILED" = 1
then exit 1
fi
