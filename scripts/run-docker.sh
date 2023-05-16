#! /bin/sh

set -e
set -x

make clean
make install

if ! tegfs config get-user admin pass
then tegfs config set-user admin --password 1234
fi

tegfs config set port 80
tegfs config set sharedir /tmp/tegfs-share

sh scripts/garbage-collector.sh &
sh scripts/preview-maker-daemon.sh &

tegfs serve --authorization
