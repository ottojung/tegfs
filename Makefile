
TEST_ROOT=build/testroot

test1: build/tegfs
	echo hi | TEGFS_ROOT=$(TEST_ROOT) build/tegfs --add \
		--registry-file testreg.tegfs.org \
		--key a 1 \
		--key b 2 \
		--key SCHEDULED 3 \


test2: build/tegfs
	TEGFS_ROOT=$(TEST_ROOT) src/scripts/save

build/tegfs: src/*.scm build
	czempak install src/tegfs.scm "$@"

build:
	mkdir -p "$@"
