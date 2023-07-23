#! /bin/sh

PASS="$(printf '%s' pass1 | sha256sum | cut '-d ' -f 1 | tr -d '\n')"

printf '' > "$TEST_ROOT/config.tegfs.lisp"
printf '(user (admin (pass . "%s")))\n' "$PASS" >> "$TEST_ROOT/config.tegfs.lisp"
printf '(fileserver . "xdg-open://")\n' >> "$TEST_ROOT/config.tegfs.lisp"
printf '(authorization . "yes")\n' >> "$TEST_ROOT/config.tegfs.lisp"
printf '(sharedir . "/tmp/tegfs-share")\n' >> "$TEST_ROOT/config.tegfs.lisp"
printf '(port . 33470)\n' >> "$TEST_ROOT/config.tegfs.lisp"
