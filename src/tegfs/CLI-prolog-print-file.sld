
(define-library
  (tegfs CLI-prolog-print-file)
  (export CLI::prolog-print-file)
  (import (only (tegfs prolog) tegfs-dump-prolog))
  (import (only (scheme base) begin define))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/CLI-prolog-print-file.scm")))
    (else (include "CLI-prolog-print-file.scm"))))
