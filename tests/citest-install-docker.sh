#! /bin/sh

. tests/common.sh

echo > dist/dockerlog

if ! make dist/dockerfile 1>>dist/dockerlog 2>>dist/dockerlog
then
    cat dist/dockerlog 1>&2
    exit 1
fi

docker run --rm -d -p 33470:80 --name tegfs tegfs 1>/dev/null

sleep 20

wget --quiet \
     --retry-on-http-error=200 \
     --retry-connrefused \
     localhost:33470 \
     -O dist/home.html

if ! cat dist/home.html | grep -q -i -e "Welcome to TegFS"
then
    echo "Docker server responded with an unexpected HTML." 1>&2
    exit 1
fi

docker exec tegfs pkill -1 guile

rm -f dist/dockerlog dist/home.html
