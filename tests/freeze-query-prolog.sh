#! /bin/sh

. tests/common.sh

CATEGORIES=$(t_tegfs --quiet categorization list-categories)

t_tegfs prolog print-file > dist/freeze-query-prolog-definitions.pl

swipl -O \
      -o dist/freeze-query-prolog.exe \
      -g main \
      -c dist/freeze-query-prolog-definitions.pl

for C1 in $CATEGORIES
do
    echo "> query $C1"
    dist/freeze-query-prolog.exe $C1 | sort
done
