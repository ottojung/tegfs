#! /bin/sh

set -e
set -x

make clean
make install

if ! tegfs config get user admin pass
then tegfs config set user admin pass --password $(cat /dev/urandom | base32 | head -c 20)
fi

tegfs config set port 80
tegfs config set sharedir /tmp/tegfs-share
tegfs config set fileserver xdg-open://

sh scripts/garbage-collector.sh &
sh scripts/preview-maker-daemon.sh &

tegfs serve
