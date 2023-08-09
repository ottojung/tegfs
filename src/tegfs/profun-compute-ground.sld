
(define-library
  (tegfs profun-compute-ground)
  (export profun-compute-ground)
  (import
    (only (euphrates profun-eval-query-terms)
          profun-eval-query/terms))
  (import
    (only (euphrates profun-standard-handler)
          profun-standard-handler))
  (import
    (only (euphrates profun)
          profun-create-falsy-database))
  (import
    (only (euphrates stack)
          stack->list
          stack-make
          stack-push!))
  (import
    (only (scheme base)
          begin
          define
          for-each
          lambda
          list))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/profun-compute-ground.scm")))
    (else (include "profun-compute-ground.scm"))))
