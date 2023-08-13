#! /bin/sh

. tests/common.sh

sh tests/freeze-big-query-prolog.sh > dist/frozen-big-query-prolog

diff dist/frozen-big-query-prolog tests/data-frozen-big-query.txt

rm -f dist/frozen-big-query-prolog
