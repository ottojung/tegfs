
(define-library
  (tegfs rule-grammar)
  (export rule-grammar)
  (import (only (tegfs tag-grammar) tag-grammar))
  (import
    (only (scheme base)
          /
          =
          begin
          define
          quasiquote
          unquote-splicing))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "tegfs/rule-grammar.scm")))
    (else (include "rule-grammar.scm"))))
