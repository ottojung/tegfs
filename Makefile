
PREFIX=/usr/local
PREFIX_BIN=$(PREFIX)/bin
PREFIX_SHARE=$(PREFIX)/share

INSTALL_ROOT=$(PREFIX_SHARE)/tegfs/root
BINARY_PATH=$(PREFIX_BIN)/tegfs
CODE_INSTALL_ROOT=$(PREFIX_SHARE)/tegfs/src
CODE_ROOT=$(PWD)/src

SUBMODULES = deps/euphrates/.git

all: dist/tegfs

build: dist/tegfs
	dist/tegfs --version 1>/dev/null

install: $(BINARY_PATH)

uninstall:
	rm -f $(BINARY_PATH)
	rm -rf $(CODE_INSTALL_ROOT)
	rmdir $(INSTALL_ROOT) || true
	rmdir $(PREFIX_SHARE)/tegfs || true

test: build test-files
	sh scripts/run-tests.sh

$(BINARY_PATH): dist/tegfs $(PREFIX_BIN)
	mkdir -p "$(INSTALL_ROOT)"
	chown "$(USER):" "$(INSTALL_ROOT)" || true
	mkdir -p "$(CODE_INSTALL_ROOT)"
	rm -rf "$(CODE_INSTALL_ROOT)"
	cp -p -T -L -r "src" "$(CODE_INSTALL_ROOT)"
	guile -s scripts/make-binary.scm "$(INSTALL_ROOT)" "$(CODE_INSTALL_ROOT)" > "$@"
	chmod +x "$@"

$(PREFIX_BIN):
	mkdir -p "$@"
	touch "$@"

reinstall: | uninstall clean install

clean:
	rm -rf dist

deps/euphrates/.git:
	git submodule update --init

dist/tegfs: $(SUBMODULES) src/*/*.scm dist
	guile -s scripts/make-binary.scm "$(TEST_ROOT_WD)" "$(CODE_ROOT)" > "$@"
	chmod +x "$@"

dist:
	mkdir -p "$@"
	touch "$@"

dist/dockerfile: dist/tegfs tests/* scripts/* assets/* deps/*
	export DOCKER_BUILDKIT=1 ; \
	docker build -f scripts/Dockerfile -t tegfs .
	touch "$@"

rundocker: dist/dockerfile
	docker run --rm -p "33470:80" --name tegfs tegfs

test-setup: build test-files

test-files: test-root

test-root:
	rsync --chmod=u+w --recursive --delete "tests/data-testroot/" "dist/testroot/"

test-files-all: test-copy-files-all

test-copy-files-all: test-root dist/rootcomplement
	rsync --chmod=u+w --recursive "dist/rootcomplement/" "dist/testroot/"

dist/dbfiles: dist/rootcomplement
	mkdir "$@"
	cp -r dist/rootcomplement/db/*/* dist/dbfiles/
	chmod -R u+w "$@"
	touch "$@" # update the glitching timestamp

dist/rootcomplement: dist/tegfs-testfiles.tar
	cd dist && tar -xf tegfs-testfiles.tar
	chmod -R a-w "$@"
	touch "$@" # update the glitching timestamp

dist/tegfs-testfiles.tar:
	mkdir -p dist # glitch in filestamps
	wget --quiet "https://vau.place/static/tegfs-testfiles.tar" -O "$@"
	touch "$@"

.PHONY: all build clean install reinstall uninstall rundocker test test-setup test-files test-root
