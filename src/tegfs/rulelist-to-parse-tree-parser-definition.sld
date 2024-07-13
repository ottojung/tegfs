
(define-library
  (tegfs rulelist-to-parse-tree-parser-definition)
  (export rulelist->parse-tree:parser:definition)
  (import
    (only (euphrates parselynn-simple)
          parselynn:simple))
  (import
    (only (tegfs rule-list-grammar)
          rule-list-grammar))
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
               "tegfs/rulelist-to-parse-tree-parser-definition.scm")))
    (else (include
            "rulelist-to-parse-tree-parser-definition.scm"))))
