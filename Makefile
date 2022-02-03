
TEST_ROOT=build/testroot

test1: build/add
	echo hi | TEGFS_ROOT=$(TEST_ROOT) build/add --registry-file testreg.tegfs.org

build/add: src/add.scm build
	czempak install src/add.scm "$@"

build:
	mkdir -p "$@"
