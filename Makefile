
TEST_ROOT=build/testroot

test1: build/add
	echo hi | TEGFS_ROOT=$(TEST_ROOT) build/add \
		--registry-file testreg.tegfs.org \
		--key a 1 \
		--key b 2 \
		--key SCHEDULED 3 \


test2: build/add
	TEGFS_ROOT=$(TEST_ROOT) src/scripts/save

build/add: src/add.scm build
	czempak install src/add.scm "$@"

build:
	mkdir -p "$@"
