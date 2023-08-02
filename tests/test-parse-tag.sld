
(define-library
  (test-parse-tag)
  (import (only (euphrates assert-equal) assert=))
  (import
    (only (euphrates assert-throw) assert-throw))
  (import (only (tegfs parse-tag) parse-tag))
  (import (only (scheme base) begin let quote))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin (include-from-path "test-parse-tag.scm")))
    (else (include "test-parse-tag.scm"))))
