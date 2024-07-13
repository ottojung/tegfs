
(define-library
  (test-make-tag-parser)
  (import (only (euphrates assert-equal) assert=))
  (import
    (only (euphrates assert-throw) assert-throw))
  (import
    (only (tegfs make-tag-parser) make-tag-parser))
  (import
    (only (scheme base)
          =>
          begin
          define
          quasiquote
          quote))
  (cond-expand
    (guile (import (only (srfi srfi-64) test-error)))
    (else (import (only (srfi 64) test-error))))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "test-make-tag-parser.scm")))
    (else (include "test-make-tag-parser.scm"))))
