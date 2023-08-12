
(define-library
  (tegfs categorization-make-choice)
  (export categorization-make-choice)
  (import
    (only (euphrates list-intersperse)
          list-intersperse))
  (import (only (euphrates tilda-a) ~a))
  (import
    (only (scheme base)
          apply
          begin
          car
          cdr
          define
          list
          map
          quote
          string-append))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/categorization-make-choice.scm")))
    (else (include "categorization-make-choice.scm"))))
