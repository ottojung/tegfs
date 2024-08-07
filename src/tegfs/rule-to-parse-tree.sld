
(define-library
  (tegfs rule-to-parse-tree)
  (export rule->parse-tree)
  (import
    (only (euphrates parselynn-simple-run)
          parselynn:simple:run))
  (import (only (euphrates raisu-star) raisu*))
  (import
    (only (tegfs rule-to-parse-tree-parser-implementation)
          rule->parse-tree:parser:implementation))
  (import
    (only (scheme base)
          begin
          cond
          define
          else
          list
          quote
          string?))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/rule-to-parse-tree.scm")))
    (else (include "rule-to-parse-tree.scm"))))
