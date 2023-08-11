
(define-library
  (tegfs dump-rules-list)
  (export dump-rules/list)
  (import
    (only (euphrates stack)
          stack->list
          stack-make
          stack-push!))
  (import (only (tegfs dump-rules) dump-rules))
  (import (only (scheme base) begin define lambda))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "tegfs/dump-rules-list.scm")))
    (else (include "dump-rules-list.scm"))))
