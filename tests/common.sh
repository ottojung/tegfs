#! /bin/sh

set -xe
make --silent test-setup -f tests/setup.make

case "$0" in
    *manualtest-*)
        make --silent test-files-all -f tests/setup.make
        ;;
esac

GUILE="guile --r7rs -L src/ -s"

TEGFS=dist/tegfs
TEGFSSEED=777

t_tegfs() {
    TEGFSSEED=$((TEGFSSEED + 10))
    $TEGFS --seed $((TEGFSSEED - 10)) "$@"
}

export TEGFS_ROOT=dist/testroot
