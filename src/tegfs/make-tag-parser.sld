
(define-library
  (tegfs make-tag-parser)
  (export
    make-tag-parser
    make-tag-structure-parser)
  (import
    (only (euphrates cartesian-product)
          cartesian-product))
  (import
    (only (euphrates lalr-parser-simple)
          lalr-parser/simple))
  (import (only (euphrates raisu-star) raisu*))
  (import (only (euphrates tilda-a) ~a))
  (import
    (only (tegfs tags-this-variable)
          tags-this-variable/string))
  (import
    (only (scheme base)
          /
          =
          begin
          cadr
          cddr
          cdr
          define
          if
          lambda
          list
          map
          null?
          quasiquote
          quote
          string->symbol
          unless
          unquote))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "tegfs/make-tag-parser.scm")))
    (else (include "make-tag-parser.scm"))))
