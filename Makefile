
PREFIX=$(HOME)/.local
PREFIX_BIN=$(PREFIX)/bin

BINARY_PATH=$(PREFIX_BIN)/tegfs

TEST_ROOT=dist/testroot
TEST_FILES=$(TEST_ROOT) $(TEST_ROOT)/categorization.tegfs.txt

SUBMODULES = deps/euphrates/.git deps/czempak/.git

CZEMPAK = CZEMPAK_ROOT=$(PWD)/.czempak-root ./dist/czempak

all: dist/tegfs

build: dist/tegfs

install: $(BINARY_PATH)

$(BINARY_PATH): dist/tegfs $(PREFIX_BIN)
	cp dist/tegfs $(PREFIX_BIN)

$(PREFIX_BIN):
	mkdir -p "$@"

reinstall: | clean install

clean:
	git submodule foreach --recursive 'git clean -dfx'
	git clean -dfx

dist/czempak: $(SUBMODULES)
	cd deps/czempak && $(MAKE) PREFIXBIN=$(PWD)/dist

deps/czempak/.git:
	git submodule update --init

deps/euphrates/.git:
	git submodule update --init

dist/tegfs: src/*.scm dist dist/czempak $(SUBMODULES)
	$(CZEMPAK) install src/tegfs.scm "$@"

dist:
	mkdir -p "$@"

.PHONY: test1 test2 test3 test4 all clean install reinstall

$(TEST_ROOT):
	mkdir -p $@

$(TEST_ROOT)/categorization.tegfs.txt:
	TEST_ROOT=$(TEST_ROOT) sh test/make-example-categorization.sh

test1: dist/tegfs $(TEST_FILES)
	touch $(TEST_ROOT)/hi.txt
	echo hi | TEGFS_ROOT=$(TEST_ROOT) dist/tegfs add \
		--target hi.txt \
		--registry-file testreg.tegfs.reg.lisp \
		--key a 1 \
		--key b 2 \
		--key SCHEDULED 3 \

test2: dist/tegfs $(TEST_FILES)
	TEGFS_ROOT=$(TEST_ROOT) dist/tegfs save

test3: dist/tegfs $(TEST_FILES)
	TEGFS_ROOT=$(TEST_ROOT) dist/tegfs categorize

test4: dist/tegfs $(TEST_FILES)
	TEST_ROOT=$(TEST_ROOT) sh test/test-serve.sh

test5: dist/tegfs $(TEST_FILES)
	TEGFS_ROOT=$(TEST_ROOT) dist/tegfs prolog

test6: dist/tegfs $(TEST_FILES)
	TEGFS_ROOT=$(TEST_ROOT) dist/tegfs query hi

test7: dist/tegfs $(TEST_FILES)
	TEGFS_ROOT=$(TEST_ROOT) dist/tegfs get "non-existent-id"
	TEGFS_ROOT=$(TEST_ROOT) dist/tegfs get "$(shell cat $(TEST_ROOT)/lastid.tegfs.txt)"
