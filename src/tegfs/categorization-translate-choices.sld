
(define-library
  (tegfs categorization-translate-choices)
  (export categorization-translate-choices)
  (import
    (only (euphrates stack)
          stack->list
          stack-make
          stack-push!))
  (import
    (only (tegfs categorization-make-choice)
          categorization-make-choice))
  (import
    (only (tegfs categorization-translate-direct-parsed-choice)
          categorization-translate-direct-parsed-choice))
  (import
    (only (tegfs unstar-symbol) unstar-symbol))
  (import
    (only (scheme base)
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
          member
          null?
          reverse))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/categorization-translate-choices.scm")))
    (else (include "categorization-translate-choices.scm"))))
