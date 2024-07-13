
(define-library
  (tegfs rule-to-parse-tree-parser-definition)
  (export rule->parse-tree:parser:definition)
  (import
    (only (euphrates parselynn-simple)
          parselynn:simple))
  (import (only (tegfs rule-grammar) rule-grammar))
  (import
    (only (scheme base)
          begin
          define
          quasiquote
          unquote))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/rule-to-parse-tree-parser-definition.scm")))
    (else (include
            "rule-to-parse-tree-parser-definition.scm"))))
