
(define-library
  (test-categorization-parse-tags)
  (import (only (euphrates assert-equal) assert=))
  (import
    (only (tegfs categorization-parse-tags)
          categorization-parse-tags))
  (import (only (scheme base) begin quote))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "test-categorization-parse-tags.scm")))
    (else (include "test-categorization-parse-tags.scm"))))
