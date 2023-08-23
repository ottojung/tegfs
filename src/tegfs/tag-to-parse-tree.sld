
(define-library
  (tegfs tag-to-parse-tree)
  (export tag->parse-tree)
  (import
    (only (euphrates lalr-parser-simple)
          lalr-parser/simple))
  (import (only (euphrates raisu-star) raisu*))
  (import (only (euphrates tilda-s) ~s))
  (import (only (tegfs tag-grammar) tag-grammar))
  (import
    (only (scheme base)
          begin
          cond
          define
          else
          list
          list?
          quasiquote
          quote
          string?
          symbol->string
          symbol?
          unquote))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "tegfs/tag-to-parse-tree.scm")))
    (else (include "tag-to-parse-tree.scm"))))
