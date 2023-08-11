
(define-library
  (tegfs categorization-parse)
  (export categorization-parse)
  (import
    (only (tegfs categorization-parse-tree)
          categorization-parse-tree))
  (import
    (only (tegfs categorization-split)
          categorization-split))
  (import
    (only (tegfs dump-rules-from-text-list)
          dump-rules-from-text/list))
  (import
    (only (tegfs inference-to-profun-rule)
          inference->profun-rule))
  (import
    (only (scheme base)
          begin
          define
          define-values
          map
          reverse
          values))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/categorization-parse.scm")))
    (else (include "categorization-parse.scm"))))
