
(define-library
  (tegfs rulelist-to-parse-tree)
  (export rulelist->parse-tree)
  (import (only (euphrates debugs) debugs))
  (import
    (only (euphrates lalr-parser-simple)
          lalr-parser/simple))
  (import (only (euphrates raisu-star) raisu*))
  (import
    (only (scheme base)
          begin
          cond
          define
          else
          list
          quasiquote
          quote
          string?
          unquote))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/rulelist-to-parse-tree.scm")))
    (else (include "rulelist-to-parse-tree.scm"))))
