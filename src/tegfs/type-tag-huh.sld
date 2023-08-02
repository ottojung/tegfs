
(define-library
  (tegfs type-tag-huh)
  (export type-tag?)
  (import (only (euphrates tilda-a) ~a))
  (import (only (scheme base) begin define or))
  (cond-expand
    (guile (import
             (only (srfi srfi-13)
                   string-prefix?
                   string-suffix?)))
    (else (import
            (only (srfi 13) string-prefix? string-suffix?))))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "tegfs/type-tag-huh.scm")))
    (else (include "type-tag-huh.scm"))))
