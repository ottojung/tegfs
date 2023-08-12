
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

test: build
	sh scripts/run-tests.sh

$(BINARY_PATH): dist/tegfs $(PREFIX_BIN)
	mkdir -p "$(INSTALL_ROOT)"
	chown "$(USER):" "$(INSTALL_ROOT)" || true
	mkdir -p "$(CODE_INSTALL_ROOT)"
	rm -rf "$(CODE_INSTALL_ROOT)"
	cp -p -T -L -r "src" "$(CODE_INSTALL_ROOT)"
	guile -s scripts/make-binary.scm "$(CODE_INSTALL_ROOT)" > "$@"
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
	guile -s scripts/make-binary.scm "$(CODE_ROOT)" > "$@"
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

.PHONY: all build clean install reinstall uninstall rundocker
