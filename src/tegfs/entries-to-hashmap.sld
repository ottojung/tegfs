
(define-library
  (tegfs entries-to-hashmap)
  (export entries->hashmap)
  (import (only (euphrates dprintln) dprintln))
  (import
    (only (euphrates hashmap)
          hashmap-set!
          make-hashmap))
  (import
    (only (tegfs entries-for-each) entries-for-each))
  (import (only (tegfs keyword-id) keyword-id))
  (import
    (only (scheme base)
          assoc
          begin
          cdr
          current-error-port
          current-output-port
          define
          if
          lambda
          let
          parameterize))
  (import (only (scheme case-lambda) case-lambda))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/entries-to-hashmap.scm")))
    (else (include "entries-to-hashmap.scm"))))
