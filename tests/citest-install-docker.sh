#! /bin/sh

. tests/common.sh

make dist/dockerfile

make rundocker

wget \
     --retry-connrefused \
     --waitretry 1 \
     --read-timeout 2 \
     --timeout 30 \
     --tries 0 \
     --output-document dist/home.html \
     -- localhost:33470

if ! cat dist/home.html | grep -q -i -e "Welcome to TegFS"
then
    echo "Docker server responded with an unexpected HTML." 1>&2
    exit 1
fi

docker stop tegfs

rm -f dist/home.html
