#! /bin/sh

. tests/common.sh

echo > dist/dockerlog

if ! make dist/dockerfile 1>>dist/dockerlog 2>>dist/dockerlog
then
    cat dist/dockerlog 1>&2
    exit 1
fi

make --silent rundocker 1>/dev/null

if ! wget --quiet \
     --retry-connrefused \
     --waitretry=1 \
     --read-timeout=2 \
     --timeout=30 \
     -t 0 \
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

docker stop tegfs 1>/dev/null

rm -f dist/dockerlog dist/home.html
