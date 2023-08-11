
(define-library
  (tegfs dump-rules-from-text-list)
  (export dump-rules-from-text/list)
  (import
    (only (euphrates stack)
          stack->list
          stack-make
          stack-push!))
  (import
    (only (tegfs dump-rules-from-text)
          dump-rules-from-text))
  (import (only (scheme base) begin define lambda))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/dump-rules-from-text-list.scm")))
    (else (include "dump-rules-from-text-list.scm"))))
