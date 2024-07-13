
(define-library
  (tegfs is-normal-tag-word-huh-parser-definition)
  (export is-normal-tag-word?:parser:definition)
  (import
    (only (euphrates parselynn-simple)
          parselynn:simple))
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
               "tegfs/is-normal-tag-word-huh-parser-definition.scm")))
    (else (include
            "is-normal-tag-word-huh-parser-definition.scm"))))
