
(define-library
  (tegfs categorization-complete-selection-cont)
  (export categorization-complete-selection/cont)
  (import
    (only (euphrates list-and-map) list-and-map))
  (import
    (only (euphrates list-deduplicate)
          list-deduplicate/reverse))
  (import
    (only (euphrates list-get-duplicates)
          list-get-duplicates))
  (import (only (euphrates negate) negate))
  (import
    (only (tegfs categorization-to-prolog-full)
          categorization->prolog/full))
  (import
    (only (tegfs categorization-translate-choices)
          categorization-translate-choices))
  (import (only (tegfs make-tag-parser) make-tag-parser))
  (import
    (only (tegfs profun-compute-ground)
          profun-compute-ground))
  (import (only (tegfs type-tag-huh) type-tag?))
  (import (only (tegfs unparse-tag) unparse-tag))
  (import
    (only (tegfs unstar-symbol) unstar-symbol))
  (import
    (only (scheme base)
          and
          append
          begin
          car
          cdr
          cons
          define
          define-values
          if
          lambda
          list
          map
          member
          not
          null?
          quasiquote
          quote))
  (cond-expand
    (guile (import (only (srfi srfi-1) filter)))
    (else (import (only (srfi 1) filter))))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/categorization-complete-selection-cont.scm")))
    (else (include
            "categorization-complete-selection-cont.scm"))))
