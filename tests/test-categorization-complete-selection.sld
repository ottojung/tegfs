
(define-library
  (test-categorization-complete-selection)
  (import (only (euphrates assert-equal) assert=))
  (import
    (only (tegfs categorization-complete-selection)
          categorization-complete-selection))
  (import (only (scheme base) begin quote))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "test-categorization-complete-selection.scm")))
    (else (include
            "test-categorization-complete-selection.scm"))))
