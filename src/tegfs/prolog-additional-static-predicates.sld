
(define-library
  (tegfs prolog-additional-static-predicates)
  (export prolog-additional-static-predicates)
  (import (only (scheme base) begin define))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/prolog-additional-static-predicates.scm")))
    (else (include
            "prolog-additional-static-predicates.scm"))))
