
(define-library
  (tegfs is-normal-tag-var-huh)
  (export is-normal-tag-var?)
  (import (only (euphrates ignore) ignore))
  (import
    (only (euphrates
            parselynn-simple-run-with-error-handler)
          parselynn:simple:run/with-error-handler))
  (import
    (only (euphrates parselynn-simple)
          parselynn:simple))
  (import (only (tegfs tag-grammar) tag-grammar))
  (import
    (only (tegfs tags-this-variable)
          tags-this-variable/string))
  (import
    (only (scheme base)
          /
          =
          begin
          define
          quasiquote
          unquote
          unquote-splicing))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/is-normal-tag-var-huh.scm")))
    (else (include "is-normal-tag-var-huh.scm"))))
