
(define-library
  (tegfs fatal)
  (export fatal)
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
  (import (only (scheme process-context) exit))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin (include-from-path "tegfs/fatal.scm")))
    (else (include "fatal.scm"))))
