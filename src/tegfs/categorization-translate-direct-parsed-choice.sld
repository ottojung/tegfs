
(define-library
  (tegfs categorization-translate-direct-parsed-choice)
  (export
    categorization-translate-direct-parsed-choice)
  (import
    (only (tegfs categorization-make-choice)
          categorization-make-choice))
  (import (only (scheme base) begin car define))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/categorization-translate-direct-parsed-choice.scm")))
    (else (include
            "categorization-translate-direct-parsed-choice.scm"))))
