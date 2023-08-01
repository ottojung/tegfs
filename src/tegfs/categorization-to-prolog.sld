
(define-library
  (tegfs categorization-to-prolog)
  (export categorization->prolog)
  (import
    (only (tegfs categorization-to-prolog-full)
          categorization->prolog/full))
  (import
    (only (scheme base) begin define define-values))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/categorization-to-prolog.scm")))
    (else (include "categorization-to-prolog.scm"))))
