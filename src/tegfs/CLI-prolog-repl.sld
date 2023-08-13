
(define-library
  (tegfs CLI-prolog-repl)
  (export CLI::prolog-repl)
  (import
    (only (euphrates file-delete) file-delete))
  (import
    (only (euphrates system-star-exit-code)
          system*/exit-code))
  (import
    (only (tegfs prolog) tegfs-dump-prolog-file))
  (import (only (scheme base) begin define))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "tegfs/CLI-prolog-repl.scm")))
    (else (include "CLI-prolog-repl.scm"))))
