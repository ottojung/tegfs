#! /bin/sh

. tests/common.sh

CATEGORIES=$(t_tegfs --quiet categorization list-categories)

for C1 in $CATEGORIES
do
    echo "> query $C1"
    t_tegfs --quiet query --format id $C1 | sort
done
