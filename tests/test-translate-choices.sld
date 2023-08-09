
(define-library
  (test-translate-choices)
  (import (only (euphrates assert-equal) assert=))
  (import
    (only (tegfs categorization-parse)
          categorization-parse))
  (import
    (only (tegfs categorization-translate-choices)
          categorization-translate-choices))
  (import
    (only (tegfs make-tag-parser) make-tag-parser))
  (import (only (scheme base) begin quote))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "test-translate-choices.scm")))
    (else (include "test-translate-choices.scm"))))
