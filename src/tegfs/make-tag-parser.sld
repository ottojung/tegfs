
(define-library
  (tegfs make-tag-parser)
  (export
    make-tag-parser
    make-tag-structure-parser)
  (import
    (only (euphrates cartesian-product)
          cartesian-product))
  (import (only (euphrates comp) comp))
  (import
    (only (euphrates lalr-parser-simple)
          lalr-parser/simple))
  (import
    (only (euphrates list-intersperse)
          list-intersperse))
  (import
    (only (euphrates list-or-map) list-or-map))
  (import
    (only (euphrates list-split-on) list-split-on))
  (import (only (euphrates raisu-star) raisu*))
  (import (only (euphrates raisu) raisu))
  (import (only (euphrates stringf) stringf))
  (import (only (euphrates tilda-a) ~a))
  (import
    (only (tegfs tag-structure-sep1)
          tag-structure-sep1))
  (import
    (only (tegfs tag-structure-sep2)
          tag-structure-sep2))
  (import
    (only (tegfs tags-this-variable)
          tags-this-variable/string))
  (import
    (only (scheme base)
          /
          =
          append
          apply
          begin
          cadr
          car
          case
          cddr
          cdr
          cond
          cons
          define
          else
          equal?
          if
          lambda
          let*
          list
          list->string
          map
          not
          null?
          quasiquote
          quote
          string->list
          string->symbol
          unless
          unquote
          unquote-splicing
          when))
  (cond-expand
    (guile (import (only (srfi srfi-13) string-null?)))
    (else (import (only (srfi 13) string-null?))))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "tegfs/make-tag-parser.scm")))
    (else (include "make-tag-parser.scm"))))
