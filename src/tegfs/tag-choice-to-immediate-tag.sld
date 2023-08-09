
(define-library
  (tegfs tag-choice-to-immediate-tag)
  (export tag-choice->immediate-tag)
  (import (only (euphrates comp) comp))
  (import (only (euphrates tilda-a) ~a))
  (import
    (only (scheme base) begin define string->symbol))
  (import (only (scheme cxr) caddr))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/tag-choice-to-immediate-tag.scm")))
    (else (include "tag-choice-to-immediate-tag.scm"))))
