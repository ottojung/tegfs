
PREFIX=/usr/local
PREFIX_BIN=$(PREFIX)/bin
PREFIX_SHARE=$(PREFIX)/share

INSTALL_ROOT=$(PREFIX_SHARE)/tegfs/root
BINARY_PATH=$(PREFIX_BIN)/tegfs
CODE_INSTALL_ROOT=$(PREFIX_SHARE)/tegfs/src
CODE_ROOT=$(PWD)/src

SUBMODULES = deps/euphrates/.git

TEST_ROOT=$(PWD)/dist/exampleroot
TEST_FILES=$(TEST_ROOT) $(TEST_ROOT)/categorization.tegfs.txt $(TEST_ROOT)/config.tegfs.lisp

all: dist/tegfs

build: dist/tegfs

install: $(BINARY_PATH)

uninstall:
	rm -f $(BINARY_PATH)
	rm -rf $(CODE_INSTALL_ROOT)
	rmdir $(INSTALL_ROOT) || true
	rmdir $(PREFIX_SHARE)/tegfs || true

$(BINARY_PATH): dist/tegfs $(PREFIX_BIN)
	mkdir -p "$(INSTALL_ROOT)"
	chown "$(USER):" "$(INSTALL_ROOT)"
	mkdir -p "$(CODE_INSTALL_ROOT)"
	rm -rf "$(CODE_INSTALL_ROOT)"
	cp -p -T -L -r "src" "$(CODE_INSTALL_ROOT)"
	guile -s scripts/make-binary.scm "$(INSTALL_ROOT)" "$(CODE_INSTALL_ROOT)" > "$@"
	chmod +x "$@"

$(PREFIX_BIN):
	mkdir -p "$@"

reinstall: | uninstall clean install

clean:
	git submodule foreach --recursive 'git clean -dfx'
	git clean -dfx

deps/euphrates/.git:
	git submodule update --init

dist/tegfs: $(SUBMODULES) src/tegfs/*.scm src/euphrates/*.scm dist
	guile -s scripts/make-binary.scm "$(TEST_ROOT)" "$(CODE_ROOT)" > "$@"
	chmod +x "$@"

dist:
	mkdir -p "$@"

dist/exampleroot.tar:
	mkdir -p dist
	wget "https://vau.place/static/tegfs-example-root.tar" -O "$@"

$(TEST_ROOT): dist/exampleroot.tar
	cd dist && tar -xf ./exampleroot.tar
	touch "$@" # update the glitching timestamp
	rm -f $(TEST_ROOT)/categorization.tegfs.txt
	rm -f $(TEST_ROOT)/config.tegfs.lisp

$(TEST_ROOT)/categorization.tegfs.txt: test/make-example-categorization.sh
	TEST_ROOT=$(TEST_ROOT) sh test/make-example-categorization.sh

$(TEST_ROOT)/config.tegfs.lisp: test/make-example-config.sh
	TEST_ROOT=$(TEST_ROOT) sh test/make-example-config.sh

test-files: $(TEST_FILES)

.PHONY: all clean install reinstall uninstall test-files
