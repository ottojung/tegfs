
PREFIX=/usr/local
PREFIX_BIN=$(PREFIX)/bin
PREFIX_SHARE=$(PREFIX)/share

BINARY_PATH=$(PREFIX_BIN)/tegfs
CODE_INSTALL_ROOT=$(PREFIX_SHARE)/tegfs/src
CODE_ROOT=$(PWD)/src

TEST_ROOT=dist/exampleroot
TEST_FILES=$(TEST_ROOT) $(TEST_ROOT)/categorization.tegfs.txt $(TEST_ROOT)/config.tegfs.lisp

SUBMODULES = deps/euphrates/.git

GUILE = guile -L $(CODE_ROOT) -s

TEST_FS = TEGFS_ROOT=$(TEST_ROOT) dist/tegfs

all: dist/tegfs

build: dist/tegfs

install: $(BINARY_PATH)

uninstall:
	rm -f $(BINARY_PATH)
	rm -rf $(CODE_INSTALL_ROOT)

$(BINARY_PATH): dist/tegfs $(PREFIX_BIN)
	mkdir -p "$(CODE_INSTALL_ROOT)"
	rm -rf "$(CODE_INSTALL_ROOT)"
	cp -p -T -L -r "src" "$(CODE_INSTALL_ROOT)"
	sed "s#$(CODE_ROOT)#$(CODE_INSTALL_ROOT)#g" dist/tegfs > "$@"
	chmod +x "$@"

$(PREFIX_BIN):
	mkdir -p "$@"

reinstall: | uninstall clean install

clean:
	git submodule foreach --recursive 'git clean -dfx'
	git clean -dfx

deps/euphrates/.git:
	git submodule update --init

dist/tegfs: src/tegfs/*.scm src/euphrates/*.scm dist $(SUBMODULES)
	echo "#! $$(command -v sh)" > "$@"
	printf "exec %s %s/tegfs/tegfs.scm \"%s%s\"\n" "$(GUILE)" "$(CODE_ROOT)" '$$' '@' >> "$@"
	chmod +x "$@"

dist:
	mkdir -p "$@"

.PHONY: test1 test2 test3 test4 all clean install reinstall uninstall

dist/exampleroot.tar:
	mkdir -p dist
	wget "https://vau.place/static/tegfs-example-root.tar" -O "$@"

$(TEST_ROOT): dist/exampleroot.tar
	cd dist && tar -xf ./exampleroot.tar
	rm -f $(TEST_ROOT)/categorization.tegfs.txt
	rm -f $(TEST_ROOT)/config.tegfs.lisp

$(TEST_ROOT)/categorization.tegfs.txt: test/make-example-categorization.sh
	TEST_ROOT=$(TEST_ROOT) sh test/make-example-categorization.sh

$(TEST_ROOT)/config.tegfs.lisp: test/make-example-config.sh
	TEST_ROOT=$(TEST_ROOT) sh test/make-example-config.sh

test-preview-daemon: dist/tegfs $(TEST_FILES)
	TEGFS_ROOT=$(TEST_ROOT) TEST_ROOT=$(TEST_ROOT) sh ./scripts/preview-maker-daemon.sh

test-make-previews: dist/tegfs $(TEST_FILES)
	TEGFS_ROOT=$(TEST_ROOT) TEST_ROOT=$(TEST_ROOT) sh ./scripts/make-all-previews.sh

test-serve: dist/tegfs $(TEST_FILES) test-make-previews
	$(TEST_FS) serve

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
	cp guix.scm dist/
	$(TEST_FS) save --target dist/guix.scm

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
	TEGFS_ROOT=$(TEST_ROOT) $(GUILE) example/rename-tag.scm

test9: dist/tegfs $(TEST_FILES)
	$(TEST_FS) query --talk
