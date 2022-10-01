#! /bin/sh

PORT=$(tegfs config get port)
SHAREDIR=$(tegfs config get sharedir)

if test -z "$PORT"
then
	echo "Garbage collector could not get the port... exiting now" 1>&2
	exit 1
fi

if test -z "$SHAREDIR"
then
	echo "Garbage collector could not get the sharedir... exiting now" 1>&2
	exit 1
fi

while true
do
	if ! wget --no-verbose "http://localhost:$PORT/collectgarbage" -O /dev/stdout
	then rm -rvf "$SHAREDIR"
	fi
	sleep 1800
done
