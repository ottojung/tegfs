
(define-library
  (tegfs is-normal-tag-word-huh)
  (export is-normal-tag-word?)
  (import (only (euphrates ignore) ignore))
  (import
    (only (euphrates
            parselynn-simple-run-with-error-handler)
          parselynn:simple:run/with-error-handler))
  (import
    (only (tegfs is-normal-tag-word-huh-parser-implementation)
          is-normal-tag-word?:parser:implementation))
  (import (only (scheme base) begin define))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/is-normal-tag-word-huh.scm")))
    (else (include "is-normal-tag-word-huh.scm"))))
