
(define-library
  (test-categorization-parse-tree)
  (import (only (euphrates assert-equal) assert=))
  (import
    (only (tegfs categorization-parse-tree)
          categorization-parse-tree))
  (import
    (only (scheme base) begin define let quote))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "test-categorization-parse-tree.scm")))
    (else (include "test-categorization-parse-tree.scm"))))
