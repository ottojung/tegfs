
PREFIX=$(HOME)/.local
PREFIX_BIN=$(PREFIX)/bin

TEST_ROOT=build/testroot

all: | submodules build/tegfs

install: all
	ln -sf $(PWD)/build/tegfs $(PREFIX_BIN)/

test1: build/tegfs
	touch $(TEST_ROOT)/hi.txt
	echo hi | TEGFS_ROOT=$(TEST_ROOT) build/tegfs add \
		--target hi.txt \
		--registry-file testreg.tegfs.org \
		--key a 1 \
		--key b 2 \
		--key SCHEDULED 3 \

submodules: deps/euphrates/src

deps/euphrates/src:
	git submodule update --init

test2: build/tegfs
	TEGFS_ROOT=$(TEST_ROOT) build/tegfs save

build/tegfs: src/*.scm build
	czempak install src/tegfs.scm "$@"

build:
	mkdir -p "$@"

.PHONY: submodules test1 test2 all install
