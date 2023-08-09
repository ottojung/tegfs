
(define-library
  (tegfs log-info)
  (export log-info)
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
          unless))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin (include-from-path "tegfs/log-info.scm")))
    (else (include "log-info.scm"))))
