
(define-library
  (tegfs make-term-parser)
  (export make-term-parser)
  (import
    (only (euphrates cartesian-product)
          cartesian-product))
  (import (only (euphrates raisu-star) raisu*))
  (import (only (euphrates raisu) raisu))
  (import (only (euphrates un-tilda-s) un~s))
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
          cons
          define
          else
          equal?
          if
          lambda
          let
          list
          map
          null?
          or
          quote
          string->number
          string->symbol
          string?
          unless))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "tegfs/make-term-parser.scm")))
    (else (include "make-term-parser.scm"))))
