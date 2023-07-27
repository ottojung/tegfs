
(define-library
  (tegfs categorization-to-prolog)
  (export categorization->prolog)
  (import
    (only (euphrates hashmap)
          hashmap-ref
          hashmap-set!
          make-hashmap))
  (import
    (only (euphrates stack)
          stack->list
          stack-make
          stack-push!))
  (import (only (euphrates tilda-a) ~a))
  (import
    (only (scheme base)
          begin
          car
          cdr
          cond
          define
          else
          equal?
          for-each
          lambda
          quasiquote
          quote
          string->symbol
          string-append
          unless
          unquote))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/categorization-to-prolog.scm")))
    (else (include "categorization-to-prolog.scm"))))
