
test-setup: test-files
	$(MAKE) build

test-files: test-root

test-root: dist
	rsync --chmod=u+w --recursive --delete "tests/data-testroot/" "dist/testroot/"

dist:
	mkdir -p "$@"
	touch "$@"

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

dist/tegfs-testfiles.tar: dist
	wget --quiet "https://vau.place/static/tegfs-testfiles.tar" -O "$@"
	touch "$@"

.PHONY: test test-setup test-files test-root test-files-all test-copy-files-all
