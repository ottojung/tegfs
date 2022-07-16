
PREFIX=$(HOME)/.local
PREFIX_BIN=$(PREFIX)/bin
PREFIX_SHARE=$(PREFIX)/share

BINARY_PATH=$(PREFIX_BIN)/tegfs
CZEMPAK_INSTALL_ROOT=$(PREFIX_SHARE)/tegfs/czempakroot/

TEST_ROOT=dist/testroot
TEST_FILES=$(TEST_ROOT) $(TEST_ROOT)/categorization.tegfs.txt $(TEST_ROOT)/config.tegfs.lisp

SUBMODULES = deps/euphrates/.git

CZEMPAK_ROOT=$(PWD)/.czempak-root

CZEMPAK = CZEMPAK_ROOT=$(CZEMPAK_ROOT) guile -s ./deps/czempak.scm

all: dist/tegfs

build: dist/tegfs

install: $(BINARY_PATH)

uninstall:
	rm -f $(BINARY_PATH)
	rm -rf $(CZEMPAK_INSTALL_ROOT)

$(BINARY_PATH): dist/tegfs $(CZEMPAK_INSTALL_ROOT) $(PREFIX_BIN)
	sed "s#$(CZEMPAK_ROOT)#$(CZEMPAK_INSTALL_ROOT)#g" dist/tegfs > "$@"
	chmod +x "$@"

$(CZEMPAK_INSTALL_ROOT):
	mkdir -p "$@"
	rm -rf "$@"
	cp -r $(CZEMPAK_ROOT) "$@"

$(PREFIX_BIN):
	mkdir -p "$@"

reinstall: | uninstall clean install

clean:
	git submodule foreach --recursive 'git clean -dfx'
	git clean -dfx

deps/euphrates/.git:
	git submodule update --init

dist/tegfs: src/*.scm dist $(SUBMODULES)
	$(CZEMPAK) install src/tegfs.scm "$@"

dist:
	mkdir -p "$@"

.PHONY: test1 test2 test3 test4 all clean install reinstall uninstall

$(TEST_ROOT):
	mkdir -p $@

$(TEST_ROOT)/categorization.tegfs.txt:
	TEST_ROOT=$(TEST_ROOT) sh test/make-example-categorization.sh

$(TEST_ROOT)/config.tegfs.lisp:
	TEST_ROOT=$(TEST_ROOT) sh test/make-example-config.sh

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

test2-m: dist/tegfs $(TEST_FILES)
	cp COPYING dist/
	TEGFS_ROOT=$(TEST_ROOT) dist/tegfs save dist/COPYING

test3: dist/tegfs $(TEST_FILES)
	TEGFS_ROOT=$(TEST_ROOT) dist/tegfs categorize

test4: dist/tegfs $(TEST_FILES)
	TEGFS_ROOT=$(TEST_ROOT) dist/tegfs serve

test5: dist/tegfs $(TEST_FILES)
	TEGFS_ROOT=$(TEST_ROOT) dist/tegfs prolog

test6: dist/tegfs $(TEST_FILES)
	TEGFS_ROOT=$(TEST_ROOT) dist/tegfs query hi

test7: dist/tegfs $(TEST_FILES)
	TEGFS_ROOT=$(TEST_ROOT) dist/tegfs get "non-existent-id"
	TEGFS_ROOT=$(TEST_ROOT) dist/tegfs get "$(shell cat $(TEST_ROOT)/lastid.tegfs.txt)"
