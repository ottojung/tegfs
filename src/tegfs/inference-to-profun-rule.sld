
(define-library
  (tegfs inference-to-profun-rule)
  (export inference->profun-rule)
  (import
    (only (tegfs generify-dumped-term)
          generify-dumped-term))
  (import
    (only (scheme base)
          append
          begin
          cadr
          cddr
          define
          list))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/inference-to-profun-rule.scm")))
    (else (include "inference-to-profun-rule.scm"))))
