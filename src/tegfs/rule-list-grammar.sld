
(define-library
  (tegfs rule-list-grammar)
  (export rule-list-grammar)
  (import (only (tegfs rule-grammar) rule-grammar))
  (import
    (only (scheme base)
          =
          begin
          define
          newline
          quasiquote
          unquote-splicing))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "tegfs/rule-list-grammar.scm")))
    (else (include "rule-list-grammar.scm"))))
