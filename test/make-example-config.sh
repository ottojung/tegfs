#! /bin/sh

PASS="$(printf '%s' pass1 | sha256sum | cut '-d ' -f 1 | tr -d '\n')"

printf '' > "$TEST_ROOT/config.tegfs.lisp"
printf '(users ((pass "%s")))\n' "$PASS" >> "$TEST_ROOT/config.tegfs.lisp"
printf '(fileserver "http://localhost:8082/")\n' >> "$TEST_ROOT/config.tegfs.lisp"
printf '(sharedir "/tmp/tegfs-share")\n' >> "$TEST_ROOT/config.tegfs.lisp"
printf '(port 8081)\n' >> "$TEST_ROOT/config.tegfs.lisp"
printf '(registries "save/main.tegfs.reg.lisp")\n' >> "$TEST_ROOT/config.tegfs.lisp"
printf '(default-save-registry "save/main.tegfs.reg.lisp")\n' >> "$TEST_ROOT/config.tegfs.lisp"
