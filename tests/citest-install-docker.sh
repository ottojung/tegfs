#! /bin/sh

. tests/common.sh

echo > dist/dockerlog

if ! make dist/dockerfile 1>>dist/dockerlog 2>>dist/dockerlog
then
    cat dist/dockerlog 1>&2
    exit 1
fi

make --silent rundocker 1>/dev/null

sleep 20

if ! wget --quiet \
     --retry-on-http-error=200 \
     --retry-connrefused \
     localhost:33470 \
     -O dist/home.html
then
    echo "Process wget failed." 1>&2
    exit 1
fi

if ! cat dist/home.html | grep -q -i -e "Welcome to TegFS"
then
    echo "Docker server responded with an unexpected HTML." 1>&2
    exit 1
fi

docker exec tegfs pkill -1 guile

rm -f dist/dockerlog dist/home.html
