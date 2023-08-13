#! /bin/sh

. tests/common.sh

CATEGORIES=$(t_tegfs --quiet categorization list-categories)

t_tegfs prolog print-file > dist/freeze-big-query-prolog-definitions.pl

swipl -O \
      -o dist/freeze-big-query-prolog.exe \
      -g main \
      -c dist/freeze-big-query-prolog-definitions.pl

for C1 in $CATEGORIES
do
    for C2 in $CATEGORIES
    do
        echo "> query $C1 $C2"
        dist/freeze-big-query-prolog.exe $C1 $C2 | sort
    done

    echo "> query $C1"
    dist/freeze-big-query-prolog.exe $C1 | sort
done
