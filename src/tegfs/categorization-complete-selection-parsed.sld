
(define-library
  (tegfs categorization-complete-selection-parsed)
  (export categorization-complete-selection/parsed)
  (import (only (euphrates comp) appcomp comp))
  (import
    (only (euphrates list-and-map) list-and-map))
  (import
    (only (euphrates list-deduplicate)
          list-deduplicate/reverse))
  (import
    (only (euphrates list-get-duplicates)
          list-get-duplicates))
  (import (only (euphrates negate) negate))
  (import (only (euphrates tilda-a) ~a))
  (import
    (only (tegfs categorization-to-prolog-full)
          categorization->prolog/full))
  (import
    (only (tegfs make-tag-parser) make-tag-parser))
  (import
    (only (tegfs profun-compute-ground)
          profun-compute-ground))
  (import
    (only (tegfs tag-choice-to-immediate-tag)
          tag-choice->immediate-tag))
  (import (only (tegfs type-tag-huh) type-tag?))
  (import (only (tegfs unparse-tag) unparse-tag))
  (import
    (only (scheme base)
          and
          append
          begin
          cadr
          car
          cdr
          cons
          define
          define-values
          equal?
          if
          lambda
          list
          map
          member
          not
          null?
          quasiquote
          quote
          string->symbol
          string-append
          symbol->string))
  (import (only (scheme cxr) caddr cdddr))
  (cond-expand
    (guile (import (only (srfi srfi-1) filter)))
    (else (import (only (srfi 1) filter))))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/categorization-complete-selection-parsed.scm")))
    (else (include
            "categorization-complete-selection-parsed.scm"))))
