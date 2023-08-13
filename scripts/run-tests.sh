#! /bin/sh

set -e

ALLFILES=$(ls tests/* || true)

for FILE in $ALLFILES
do
	case "$FILE" in
		*.sld) ;;
		*.sh) ;;
		*) continue ;;
	esac

	case "$FILE" in
		tests/test-*) ;;
		tests/citest-*)
			case "$CI" in
				1) ;;
				*) continue ;;
			esac
			;;
		*) continue ;;
	esac

	FILES="$FILES $FILE"
done

TESTCOUNT=$(echo "$FILES" | wc -w)
INDEX=0
FAILED=0

for FILE in $FILES
do
	INDEX=$((INDEX + 1))
	SHORT="$(basename "$FILE")"

	printf '(%s/%s)' "$INDEX" "$TESTCOUNT"
	printf ' %s ... ' "$SHORT"

	case "$FILE" in

		*.sh)
			if sh "$FILE" 2>"dist/testlog-$INDEX.txt" 1>"dist/testlog-$INDEX.txt"
			then printf '✓'
			else
				printf 'X\n'
				printf ' ----------------\n'
				cat "dist/testlog-$INDEX.txt"
				printf ' ----------------\n'
				FAILED=1
			fi
			printf '\n'
			;;

		*.sld)
			R=$(if guile --r7rs -L "src" -L "tests" -s "$FILE" 2>&1
			    then printf '✓'
			    else printf 'X'
			    fi)
			printf '%s' "$R" | grep -v -e '^;;; ' || true
			printf '%s' "$R" | grep -q -e '✓' || FAILED=1
			;;

	esac
done

if test "$FAILED" = 1
then exit 1
fi
