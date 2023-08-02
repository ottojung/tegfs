
(define-library
  (tegfs categorization-translate-choices)
  (export categorization-translate-choices)
  (import
    (only (euphrates stack)
          stack->list
          stack-make
          stack-push!))
  (import
    (only (tegfs mangle-tag-choice)
          mangle-tag-choice))
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
          member))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/categorization-translate-choices.scm")))
    (else (include "categorization-translate-choices.scm"))))
