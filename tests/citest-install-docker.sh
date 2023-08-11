#! /bin/sh

. tests/common.sh

echo > log

if ! make dist/dockerfile
then
    cat log 1>&2
    exit 1
fi

docker run --rm -d -p 33470:80 --name tegfs tegfs 1>/dev/null

sleep 10

wget --quiet \
     --retry-on-http-error=200 \
     --retry-connrefused \
     localhost:33470 \
     -O home.html

if ! cat home.html | grep -q -i -e "Welcome to TegFS"
then
    echo "Docker server responded with an unexpected HTML." 1>&2
    exit 1
fi

docker exec tegfs pkill -1 guile

rm -f log home.html
