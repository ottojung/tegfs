
(define-library
  (tegfs log-question)
  (export log-question)
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
           (begin
             (include-from-path "tegfs/log-question.scm")))
    (else (include "log-question.scm"))))
