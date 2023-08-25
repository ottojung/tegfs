
(define-library
  (tegfs unparse-tag-var)
  (export unparse-tag-var)
  (import (only (euphrates tilda-a) ~a))
  (import (only (euphrates tilda-s) ~s))
  (import
    (only (tegfs is-normal-tag-var-huh)
          is-normal-tag-var?))
  (import (only (scheme base) begin define if))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "tegfs/unparse-tag-var.scm")))
    (else (include "unparse-tag-var.scm"))))
