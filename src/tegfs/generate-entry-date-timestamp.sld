
(define-library
  (tegfs generate-entry-date-timestamp)
  (export generate-entry-date-timestamp)
  (import
    (only (euphrates date-get-current-string)
          date-get-current-string))
  (import (only (scheme base) begin define))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/generate-entry-date-timestamp.scm")))
    (else (include "generate-entry-date-timestamp.scm"))))
