
(define-library
  (tegfs fatal)
  (export fatal)
  (import (only (tegfs log-error) log-error))
  (import
    (only (scheme base) apply begin cons define))
  (import (only (scheme process-context) exit))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin (include-from-path "tegfs/fatal.scm")))
    (else (include "fatal.scm"))))
