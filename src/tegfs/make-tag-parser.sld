
(define-library
  (tegfs make-tag-parser)
  (export
    make-tag-parser
    make-tag-structure-parser)
  (import (only (euphrates tilda-a) ~a))
  (import
    (only (tegfs make-term-parser) make-term-parser))
  (import
    (only (scheme base)
          begin
          car
          cdr
          cons
          define
          if
          lambda
          map
          or
          string?
          symbol?))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "tegfs/make-tag-parser.scm")))
    (else (include "make-tag-parser.scm"))))
