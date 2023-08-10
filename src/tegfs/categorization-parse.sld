
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
    (only (scheme base) begin define define-values))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/categorization-parse.scm")))
    (else (include "categorization-parse.scm"))))
