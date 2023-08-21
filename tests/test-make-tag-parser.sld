
(define-library
  (test-make-tag-parser)
  (import (only (euphrates assert-equal) assert=))
  (import
    (only (euphrates assert-throw) assert-throw))
  (import
    (only (tegfs make-tag-parser) make-tag-parser))
  (import (only (scheme base) begin let quote))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "test-make-tag-parser.scm")))
    (else (include "test-make-tag-parser.scm"))))
