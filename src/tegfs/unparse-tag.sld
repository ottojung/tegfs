
(define-library
  (tegfs unparse-tag)
  (export unparse-tag)
  (import
    (only (euphrates list-intersperse)
          list-intersperse))
  (import
    (only (euphrates list-singleton-q)
          list-singleton?))
  (import (only (euphrates tilda-a) ~a))
  (import
    (only (tegfs tags-this-variable)
          tags-this-variable/string))
  (import
    (only (tegfs unparse-tag-var) unparse-tag-var))
  (import
    (only (tegfs unparse-tag-word) unparse-tag-word))
  (import
    (only (scheme base)
          and
          apply
          begin
          car
          cdr
          define
          equal?
          if
          map
          null?
          or
          quasiquote
          string-append
          unquote
          unquote-splicing))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "tegfs/unparse-tag.scm")))
    (else (include "unparse-tag.scm"))))
