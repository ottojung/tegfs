
(define-library
  (tegfs entries-to-hashmap)
  (export entries->hashmap)
  (import
    (only (euphrates hashmap)
          hashmap-set!
          make-hashmap))
  (import
    (only (tegfs entries-for-each) entries-for-each))
  (import (only (tegfs keyword-id) keyword-id))
  (import (only (tegfs log-warning) log-warning))
  (import
    (only (scheme base)
          assoc
          begin
          cdr
          define
          if
          lambda
          let))
  (import (only (scheme case-lambda) case-lambda))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/entries-to-hashmap.scm")))
    (else (include "entries-to-hashmap.scm"))))
