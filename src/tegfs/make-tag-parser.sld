
(define-library
  (tegfs make-tag-parser)
  (export
    make-tag-parser
    make-tag-structure-parser)
  (import
    (only (euphrates cartesian-product)
          cartesian-product))
  (import (only (euphrates raisu-star) raisu*))
  (import (only (euphrates raisu) raisu))
  (import
    (only (tegfs tag-to-parse-tree) tag->parse-tree))
  (import
    (only (tegfs tags-this-variable)
          tags-this-variable/string))
  (import
    (only (scheme base)
          begin
          cadr
          car
          cddr
          cdr
          cond
          define
          else
          equal?
          if
          lambda
          let
          list
          map
          null?
          quote
          string->symbol
          unless))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "tegfs/make-tag-parser.scm")))
    (else (include "make-tag-parser.scm"))))
