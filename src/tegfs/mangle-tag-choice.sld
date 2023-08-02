
(define-library
  (tegfs mangle-tag-choice)
  (export mangle-tag-choice)
  (import (only (euphrates tilda-a) ~a))
  (import
    (only (scheme base) begin define string-append))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "tegfs/mangle-tag-choice.scm")))
    (else (include "mangle-tag-choice.scm"))))
