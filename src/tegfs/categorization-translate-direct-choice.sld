
(define-library
  (tegfs categorization-translate-direct-choice)
  (export categorization-translate-direct-choice)
  (import
    (only (tegfs categorization-translate-direct-parsed-choice)
          categorization-translate-direct-parsed-choice))
  (import
    (only (tegfs unstar-symbol) unstar-symbol))
  (import (only (scheme base) begin define map))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/categorization-translate-direct-choice.scm")))
    (else (include
            "categorization-translate-direct-choice.scm"))))
