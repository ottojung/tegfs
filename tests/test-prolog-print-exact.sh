#! /bin/sh

. tests/common.sh

t_tegfs --quiet prolog print-file > dist/test-prolog-file.pro

diff dist/test-prolog-file.pro tests/data-test-prolog-file.pro
