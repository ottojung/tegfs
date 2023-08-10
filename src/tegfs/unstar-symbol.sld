
(define-library
  (tegfs unstar-symbol)
  (export unstar-symbol)
  (import (only (euphrates comp) comp))
  (import (only (euphrates negate) negate))
  (import (only (euphrates tilda-a) ~a))
  (import
    (only (scheme base)
          begin
          define
          equal?
          list->string
          string->list
          string->symbol))
  (cond-expand
    (guile (import (only (srfi srfi-1) filter)))
    (else (import (only (srfi 1) filter))))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "tegfs/unstar-symbol.scm")))
    (else (include "unstar-symbol.scm"))))
