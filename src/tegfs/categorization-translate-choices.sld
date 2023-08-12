
(define-library
  (tegfs categorization-translate-choices)
  (export categorization-translate-choices)
  (import
    (only (euphrates list-intersperse)
          list-intersperse))
  (import
    (only (euphrates stack)
          stack->list
          stack-make
          stack-push!))
  (import (only (euphrates tilda-a) ~a))
  (import
    (only (tegfs categorization-translate-direct-parsed-choice)
          categorization-translate-direct-parsed-choice))
  (import
    (only (tegfs unstar-symbol) unstar-symbol))
  (import
    (only (scheme base)
          append
          apply
          begin
          car
          cdr
          cond
          define
          equal?
          for-each
          if
          lambda
          let
          list
          map
          member
          quote
          reverse
          string-append))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/categorization-translate-choices.scm")))
    (else (include "categorization-translate-choices.scm"))))
