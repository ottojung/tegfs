
(define-library
  (tegfs verbosity-level-p)
  (export verbosity-level/p)
  (import
    (only (scheme base) begin define make-parameter))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "tegfs/verbosity-level-p.scm")))
    (else (include "verbosity-level-p.scm"))))
