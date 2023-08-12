
(define-library
  (tegfs log-error)
  (export log-error)
  (import (only (euphrates dprintln) dprintln))
  (import
    (only (tegfs verbosity-level-p)
          verbosity-level/p))
  (import
    (only (scheme base)
          >
          apply
          begin
          cons
          current-error-port
          current-output-port
          define
          parameterize
          string-append
          unless))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin (include-from-path "tegfs/log-error.scm")))
    (else (include "log-error.scm"))))
