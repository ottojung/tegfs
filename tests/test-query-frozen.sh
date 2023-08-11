#! /bin/sh

. tests/common.sh

sh tests/freeze-query.sh > frozen-query

diff frozen-query tests/data-frozen-query.txt

rm -f frozen-query
