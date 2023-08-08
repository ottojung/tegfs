
(define-library
  (tegfs make-term-parser)
  (export make-term-parser)
  (import (only (euphrates tilda-a) ~a))
  (import (only (tegfs make-tag-parser) make-tag-parser))
  (import
    (only (tegfs tags-this-variable)
          tags-this-variable/string))
  (import
    (only (scheme base)
          begin
          car
          cdr
          cons
          define
          equal?
          if
          lambda
          map
          string->symbol))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "tegfs/make-term-parser.scm")))
    (else (include "make-term-parser.scm"))))
