
(define-library
  (tegfs is-normal-tag-word-huh)
  (export is-normal-tag-word?)
  (import (only (euphrates ignore) ignore))
  (import
    (only (euphrates lalr-parser-simple)
          lalr-parser/simple))
  (import (only (tegfs tag-grammar) tag-grammar))
  (import
    (only (scheme base)
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
               "tegfs/is-normal-tag-word-huh.scm")))
    (else (include "is-normal-tag-word-huh.scm"))))
