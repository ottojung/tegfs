
(define-library
  (tegfs CLI-swiched-field-huh-p)
  (export CLI::swiched-field?/p)
  (import
    (only (scheme base) begin define make-parameter))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/CLI-swiched-field-huh-p.scm")))
    (else (include "CLI-swiched-field-huh-p.scm"))))
