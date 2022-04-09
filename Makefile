
PREFIX=$(HOME)/.local
PREFIX_BIN=$(PREFIX)/bin

BINARY_PATH=$(PREFIX_BIN)/tegfs

TEST_ROOT=build/testroot

SUBMODULES = deps/euphrates/.git deps/czempak/.git

CZEMPAK = CZEMPAK_ROOT=$(PWD)/.czempak-root ./build/czempak

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

build/czempak: $(SUBMODULES)
	cd deps/czempak && $(MAKE) PREFIXBIN=$(PWD)/build

deps/czempak/.git:
	git submodule update --init

deps/euphrates/.git:
	git submodule update --init

build/tegfs: src/*.scm build build/czempak $(SUBMODULES)
	$(CZEMPAK) install src/tegfs.scm "$@"

build:
	mkdir -p "$@"

.PHONY: test1 test2 test3 test4 all clean install reinstall

test1: build/tegfs
	touch $(TEST_ROOT)/hi.txt
	echo hi | TEGFS_ROOT=$(TEST_ROOT) build/tegfs add \
		--target hi.txt \
		--registry-file testreg.tegfs.org \
		--key a 1 \
		--key b 2 \
		--key SCHEDULED 3 \

test2: build/tegfs
	TEGFS_ROOT=$(TEST_ROOT) build/tegfs save

test3: build/tegfs
	TEGFS_ROOT=$(TEST_ROOT) build/tegfs categorize

test4: build/tegfs
	printf '(user (pass "' > $(TEST_ROOT)/auth.tegfs.lisp
	printf '%s' pass1 | sha256sum | cut '-d ' -f 1 | tr -d '\n' >> $(TEST_ROOT)/auth.tegfs.lisp
	printf '"))\n' >> $(TEST_ROOT)/auth.tegfs.lisp
	TEGFS_ROOT=$(TEST_ROOT) build/tegfs serve

test5: build/tegfs
	TEGFS_ROOT=$(TEST_ROOT) build/tegfs prolog

test6: build/tegfs
	TEGFS_ROOT=$(TEST_ROOT) build/tegfs query hi
