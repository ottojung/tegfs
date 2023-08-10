
(define-library
  (tegfs categorization-translate-direct-parsed-choice)
  (export
    categorization-translate-direct-parsed-choice)
  (import (only (euphrates tilda-a) ~a))
  (import
    (only (scheme base)
          append
          begin
          car
          define
          list
          map
          quote))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/categorization-translate-direct-parsed-choice.scm")))
    (else (include
            "categorization-translate-direct-parsed-choice.scm"))))
