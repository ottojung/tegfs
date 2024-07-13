
(define-library
  (tegfs tag-to-parse-tree-parser-definition)
  (export tag->parse-tree:parser:definition)
  (import
    (only (euphrates parselynn-simple)
          parselynn:simple))
  (import (only (tegfs tag-grammar) tag-grammar))
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
               "tegfs/tag-to-parse-tree-parser-definition.scm")))
    (else (include
            "tag-to-parse-tree-parser-definition.scm"))))
