
(define-library
  (tegfs rulelist-to-parse-tree)
  (export rulelist->parse-tree)
  (import
    (only (euphrates parselynn-simple-run)
          parselynn:simple:run))
  (import (only (euphrates raisu-star) raisu*))
  (import
    (only (tegfs rulelist-to-parse-tree-parser-implementation)
          rulelist->parse-tree:parser:implementation))
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
               "tegfs/rulelist-to-parse-tree.scm")))
    (else (include "rulelist-to-parse-tree.scm"))))
