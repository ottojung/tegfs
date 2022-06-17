#! /bin/sh

PORT=$(tegfs config get port)

if test -z "$PORT"
then
	echo "Garbage collector could not get the port... exiting now" 1>&2
	exit 1
fi

while true
do
	curl --no-progress-meter "http://localhost:$PORT/collectgarbage"
	sleep 1800
done
