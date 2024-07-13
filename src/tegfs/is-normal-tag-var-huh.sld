
(define-library
  (tegfs is-normal-tag-var-huh)
  (export is-normal-tag-var?)
  (import (only (euphrates ignore) ignore))
  (import
    (only (euphrates
            parselynn-simple-run-with-error-handler)
          parselynn:simple:run/with-error-handler))
  (import
    (only (tegfs is-normal-tag-var-huh-parser-implementation)
          is-normal-tag-var?:parser:implementation))
  (import (only (scheme base) begin define))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/is-normal-tag-var-huh.scm")))
    (else (include "is-normal-tag-var-huh.scm"))))
