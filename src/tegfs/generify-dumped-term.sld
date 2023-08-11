
(define-library
  (tegfs generify-dumped-term)
  (export generify-dumped-term)
  (import (only (euphrates tilda-a) ~a))
  (import
    (only (tegfs prolog-var)
          prolog-var-name
          prolog-var?))
  (import
    (only (scheme base)
          begin
          cdr
          cond
          define
          else
          if
          lambda
          let
          map
          string->symbol
          symbol?))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/generify-dumped-term.scm")))
    (else (include "generify-dumped-term.scm"))))
