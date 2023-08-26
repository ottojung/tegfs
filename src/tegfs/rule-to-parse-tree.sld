
(define-library
  (tegfs rule-to-parse-tree)
  (export rule->parse-tree)
  (import (only (euphrates debugs) debugs))
  (import
    (only (euphrates lalr-parser-simple)
          lalr-parser/simple))
  (import (only (euphrates raisu-star) raisu*))
  (import (only (tegfs rule-grammar) rule-grammar))
  (import
    (only (scheme base)
          begin
          cond
          define
          else
          list
          quasiquote
          quote
          string?
          unquote))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/rule-to-parse-tree.scm")))
    (else (include "rule-to-parse-tree.scm"))))
