#! /bin/sh

set -x

printf '(users (user ((pass "' > "$TEST_ROOT/config.tegfs.lisp"
printf '%s' pass1 | sha256sum | cut '-d ' -f 1 | tr -d '\n' >> "$TEST_ROOT/config.tegfs.lisp"
printf '"))))\n' >> "$TEST_ROOT/config.tegfs.lisp"
TEGFS_FILESERVER="http://localhost:8082/" TEGFS_SHAREDIR="/tmp/tegfs-share" TEGFS_ROOT="$TEST_ROOT" \
	dist/tegfs serve
