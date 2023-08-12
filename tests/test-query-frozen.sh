#! /bin/sh

. tests/common.sh

sh tests/freeze-query.sh > dist/frozen-query

diff dist/frozen-query tests/data-frozen-query.txt

rm -f dist/frozen-query
