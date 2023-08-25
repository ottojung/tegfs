
(define-library
  (tegfs unparse-tag-word)
  (export unparse-tag-word)
  (import (only (euphrates tilda-s) ~s))
  (import
    (only (tegfs is-normal-tag-word-huh)
          is-normal-tag-word?))
  (import (only (scheme base) begin define if))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "tegfs/unparse-tag-word.scm")))
    (else (include "unparse-tag-word.scm"))))
