#! /bin/sh

set -x

printf '(user (pass "' > "$TEST_ROOT/auth.tegfs.lisp"
printf '%s' pass1 | sha256sum | cut '-d ' -f 1 | tr -d '\n' >> "$TEST_ROOT/auth.tegfs.lisp"
printf '"))\n' >> "$TEST_ROOT/auth.tegfs.lisp"
TEGFS_FILESERVER="http://localhost:8080/" TEGFS_SHAREDIR="/tmp/tegfs-share" TEGFS_ROOT="$TEST_ROOT" \
	dist/tegfs serve
