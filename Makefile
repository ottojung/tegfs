
PREFIX=$(HOME)/.local
PREFIX_BIN=$(PREFIX)/bin

BINARY_PATH=$(PREFIX_BIN)/tegfs

TEST_ROOT=build/testroot

SUBMODULES=deps/euphrates/src

all: build/tegfs

install: $(BINARY_PATH)

$(BINARY_PATH): build/tegfs $(PREFIX_BIN)
	cp $(PWD)/build/tegfs $(PREFIX_BIN)

$(PREFIX_BIN):
	mkdir -p "$@"

reinstall: | clean install

clean:
	git submodule foreach --recursive 'git clean -dfx'
	git clean -dfx

test1: build/tegfs
	touch $(TEST_ROOT)/hi.txt
	echo hi | TEGFS_ROOT=$(TEST_ROOT) build/tegfs add \
		--target hi.txt \
		--registry-file testreg.tegfs.org \
		--key a 1 \
		--key b 2 \
		--key SCHEDULED 3 \

deps/euphrates/src:
	git submodule update --init

test2: build/tegfs
	TEGFS_ROOT=$(TEST_ROOT) build/tegfs save

test3: build/tegfs
	TEGFS_ROOT=$(TEST_ROOT) build/tegfs categorize

build/tegfs: src/*.scm build $(SUBMODULES)
	czempak install src/tegfs.scm "$@"

build:
	mkdir -p "$@"

.PHONY: test1 test2 all clean install reinstall
