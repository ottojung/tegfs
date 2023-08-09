
(define-library
  (tegfs sharedinfo-time-left)
  (export sharedinfo-time-left)
  (import
    (only (tegfs current-time-p) current-time/p))
  (import
    (only (tegfs sharedinfo)
          sharedinfo-date
          sharedinfo-stime))
  (import
    (only (scheme base) + - begin define max))
  (import (only (scheme case-lambda) case-lambda))
  (cond-expand
    (guile (import (only (srfi srfi-18) current-time)))
    (else (import (only (srfi 18) current-time))))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/sharedinfo-time-left.scm")))
    (else (include "sharedinfo-time-left.scm"))))
