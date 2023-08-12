#! /bin/sh

. tests/common.sh

CATEGORIES=$($TEGFS --quiet categorization list-categories)

for C1 in $CATEGORIES
do
    for C2 in $CATEGORIES
    do
        echo "> query $C1 $C2"
        $TEGFS --quiet query --format id $C1 $C2 | sort
    done

    echo "> query $C1"
    $TEGFS --quiet query --format id $C1 | sort
done
