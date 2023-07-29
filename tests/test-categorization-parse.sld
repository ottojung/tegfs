
(define-library
  (test-categorization-parse)
  (import (only (euphrates assert-equal) assert=))
  (import
    (only (tegfs categorization-parse)
          categorization-parse))
  (import (only (scheme base) begin quote))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "test-categorization-parse.scm")))
    (else (include "test-categorization-parse.scm"))))
