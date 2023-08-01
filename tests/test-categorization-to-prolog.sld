
(define-library
  (test-categorization-to-prolog)
  (import (only (euphrates assert-equal) assert=))
  (import
    (only (tegfs categorization-parse)
          categorization-parse))
  (import
    (only (tegfs categorization-to-prolog)
          categorization->prolog))
  (import (only (scheme base) begin quote))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "test-categorization-to-prolog.scm")))
    (else (include "test-categorization-to-prolog.scm"))))
