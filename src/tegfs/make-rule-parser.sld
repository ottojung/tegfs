
(define-library
  (tegfs make-rule-parser)
  (export make-rule-parser)
  (import
    (only (euphrates cartesian-product)
          cartesian-product))
  (import (only (euphrates negate) negate))
  (import (only (euphrates raisu-star) raisu*))
  (import
    (only (tegfs make-term-parser) make-term-parser))
  (import
    (only (tegfs rule-to-parse-tree)
          rule->parse-tree))
  (import
    (only (scheme base)
          append
          apply
          begin
          cadr
          cdr
          define
          lambda
          list
          list-ref
          map
          quote
          string?
          unless))
  (cond-expand
    (guile (import (only (srfi srfi-1) filter)))
    (else (import (only (srfi 1) filter))))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "tegfs/make-rule-parser.scm")))
    (else (include "make-rule-parser.scm"))))
