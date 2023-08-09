
(define-library
  (tegfs sharedinfo-still-valid-huh)
  (export sharedinfo-still-valid?)
  (import
    (only (tegfs current-time-p) current-time/p))
  (import
    (only (tegfs sharedinfo-time-left)
          sharedinfo-time-left))
  (import (only (scheme base) < begin define))
  (import (only (scheme case-lambda) case-lambda))
  (cond-expand
    (guile (import (only (srfi srfi-18) current-time)))
    (else (import (only (srfi 18) current-time))))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/sharedinfo-still-valid-huh.scm")))
    (else (include "sharedinfo-still-valid-huh.scm"))))
