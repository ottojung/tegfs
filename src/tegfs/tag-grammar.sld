
(define-library
  (tegfs tag-grammar)
  (export tag-grammar)
  (import
    (only (tegfs tags-this-variable)
          tags-this-variable/string))
  (import
    (only (scheme base)
          /
          =
          begin
          define
          quasiquote
          string
          unquote))
  (cond-expand
    (guile (import (only (srfi srfi-1) any)))
    (else (import (only (srfi 1) any))))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "tegfs/tag-grammar.scm")))
    (else (include "tag-grammar.scm"))))
