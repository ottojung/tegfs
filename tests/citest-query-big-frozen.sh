#! /bin/sh

. tests/common.sh

sh tests/freeze-big-query.sh > dist/frozen-big-query

diff dist/frozen-big-query tests/data-frozen-big-query.txt

rm -f dist/frozen-big-query
