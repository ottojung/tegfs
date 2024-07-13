
(define-library
  (tegfs tag-to-parse-tree)
  (export tag->parse-tree)
  (import
    (only (euphrates
            parselynn-simple-run-with-error-handler)
          parselynn:simple:run/with-error-handler))
  (import (only (euphrates raisu-star) raisu*))
  (import (only (euphrates tilda-s) ~s))
  (import
    (only (tegfs tag-to-parse-tree-parser-implementation)
          tag->parse-tree:parser:implementation))
  (import
    (only (scheme base)
          begin
          cond
          define
          else
          list
          list?
          quote
          string?
          symbol->string
          symbol?))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "tegfs/tag-to-parse-tree.scm")))
    (else (include "tag-to-parse-tree.scm"))))
