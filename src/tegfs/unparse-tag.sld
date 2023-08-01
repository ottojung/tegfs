
(define-library
  (tegfs unparse-tag)
  (export unparse-tag)
  (import
    (only (euphrates list-intersperse)
          list-intersperse))
  (import (only (euphrates tilda-a) ~a))
  (import
    (only (tegfs tags-this-variable)
          tags-this-variable/string))
  (import
    (only (scheme base)
          and
          apply
          begin
          car
          cdr
          cond
          define
          else
          equal?
          map
          null?
          quasiquote
          string->symbol
          string-append
          symbol->string
          unquote
          unquote-splicing))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "tegfs/unparse-tag.scm")))
    (else (include "unparse-tag.scm"))))
