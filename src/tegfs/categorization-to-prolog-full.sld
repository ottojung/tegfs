
(define-library
  (tegfs categorization-to-prolog-full)
  (export categorization->prolog/full)
  (import (only (euphrates comp) comp))
  (import
    (only (euphrates hashmap)
          hashmap->alist
          hashmap-ref
          hashmap-set!
          make-hashmap))
  (import
    (only (euphrates list-deduplicate)
          list-deduplicate/reverse))
  (import
    (only (euphrates list-singleton-q)
          list-singleton?))
  (import (only (euphrates negate) negate))
  (import
    (only (euphrates stack)
          stack->list
          stack-make
          stack-push!))
  (import
    (only (tegfs unstar-symbol) unstar-symbol))
  (import
    (only (scheme base)
          begin
          car
          cdr
          cons
          define
          equal?
          for-each
          if
          lambda
          let
          map
          null?
          or
          quasiquote
          quote
          symbol->string
          unless
          unquote
          values
          when))
  (cond-expand
    (guile (import (only (srfi srfi-1) filter)))
    (else (import (only (srfi 1) filter))))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/categorization-to-prolog-full.scm")))
    (else (include "categorization-to-prolog-full.scm"))))
