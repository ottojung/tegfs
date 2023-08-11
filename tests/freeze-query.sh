#! /bin/sh

. tests/common.sh

CATEGORIES=$($TEST_FS --quiet categorization list-categories)

for C1 in $CATEGORIES
do
    for C2 in $CATEGORIES
    do
        echo "> query $C1 $C2"
        $TEST_FS --quiet query --format id $C1 $C2 | sort
    done

    echo "> query $C1"
    $TEST_FS --quiet query --format id $C1 | sort
done
