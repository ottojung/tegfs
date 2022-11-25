
PREFIX=/usr/local
PREFIX_BIN=$(PREFIX)/bin
PREFIX_SHARE=$(PREFIX)/share

BINARY_PATH=$(PREFIX_BIN)/tegfs
CZEMPAK_INSTALL_ROOT=$(PREFIX_SHARE)/tegfs/czempakroot

TEST_ROOT=dist/testroot
TEST_FILES=$(TEST_ROOT) $(TEST_ROOT)/categorization.tegfs.txt $(TEST_ROOT)/config.tegfs.lisp

SUBMODULES = deps/euphrates/.git

CZEMPAK_ROOT=$(PWD)/.czempak-root

CZEMPAK = CZEMPAK_ROOT=$(CZEMPAK_ROOT) guile -s ./deps/czempak.scm

TEST_FS = TEGFS_ROOT=$(TEST_ROOT) dist/tegfs

all: dist/tegfs

build: dist/tegfs

install: $(BINARY_PATH)

uninstall:
	rm -f $(BINARY_PATH)
	rm -rf $(CZEMPAK_INSTALL_ROOT)

$(BINARY_PATH): dist/tegfs $(PREFIX_BIN)
	mkdir -p "$(CZEMPAK_INSTALL_ROOT)"
	rm -rf "$(CZEMPAK_INSTALL_ROOT)"
	cp -p -T -r "$(CZEMPAK_ROOT)" "$(CZEMPAK_INSTALL_ROOT)"
	sed "s#$(CZEMPAK_ROOT)#$(CZEMPAK_INSTALL_ROOT)#g" dist/tegfs > "$@"
	chmod +x "$@"

$(PREFIX_BIN):
	mkdir -p "$@"

reinstall: | uninstall clean install

clean:
	git submodule foreach --recursive 'git clean -dfx'
	git clean -dfx

deps/euphrates/.git:
	git submodule update --init

dist/tegfs: src/*.scm src/euphrates/*.scm dist $(SUBMODULES)
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
	echo hi | $(TEST_FS) add \
		--target hi.txt \
		--registry-file testreg.tegfs.reg.lisp \
		--key a 1 \
		--key b 2 \
		--key SCHEDULED 3 \

test2: dist/tegfs $(TEST_FILES)
	$(TEST_FS) save

test2-m: dist/tegfs $(TEST_FILES)
	cp COPYING dist/
	$(TEST_FS) save dist/COPYING

test3: dist/tegfs $(TEST_FILES)
	$(TEST_FS) categorize

test4: dist/tegfs $(TEST_FILES)
	$(TEST_FS) serve

test5: dist/tegfs $(TEST_FILES)
	$(TEST_FS) prolog

test6: dist/tegfs $(TEST_FILES)
	$(TEST_FS) query hi

test7: dist/tegfs $(TEST_FILES)
	$(TEST_FS) get "non-existent-id"
	$(TEST_FS) get "$(shell cat $(TEST_ROOT)/lastid.tegfs.txt)"

test8: dist/tegfs $(TEST_FILES)
	TEGFS_ROOT=$(TEST_ROOT) $(CZEMPAK) run example/rename-tag.scm

test9: dist/tegfs $(TEST_FILES)
	$(TEST_FS) query --talk
