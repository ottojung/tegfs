#! /bin/sh

TARGET_ROOT="$1"
if test -z "$TARGET_ROOT"
then
    echo "Expected root directory as first argument" 1>&2
    exit 1
fi

mkdir -p "$TARGET_ROOT"

PASS="$(printf '%s' pass1 | sha256sum | cut '-d ' -f 1 | tr -d '\n')"

printf '' > "$TARGET_ROOT/config.tegfs.lisp"
printf '(user (admin (pass . "%s")))\n' "$PASS" >> "$TARGET_ROOT/config.tegfs.lisp"
printf '(fileserver . "xdg-open://")\n' >> "$TARGET_ROOT/config.tegfs.lisp"
printf '(authorization . "yes")\n' >> "$TARGET_ROOT/config.tegfs.lisp"
printf '(sharedir . "/tmp/tegfs-share")\n' >> "$TARGET_ROOT/config.tegfs.lisp"
printf '(port . 33470)\n' >> "$TARGET_ROOT/config.tegfs.lisp"
