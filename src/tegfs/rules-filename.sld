
(define-library
  (tegfs rules-filename)
  (export rules-filename)
  (import
    (only (scheme base) begin cond-expand define))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "tegfs/rules-filename.scm")))
    (else (include "rules-filename.scm"))))
