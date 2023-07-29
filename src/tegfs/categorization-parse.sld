
(define-library
  (tegfs categorization-parse)
  (export categorization-parse)
  (import
    (only (euphrates call-with-input-string)
          call-with-input-string))
  (import (only (euphrates curry-if) curry-if))
  (import (only (euphrates identity) identity))
  (import
    (only (euphrates list-deduplicate)
          list-deduplicate))
  (import
    (only (euphrates list-map-flatten)
          list-map/flatten))
  (import
    (only (euphrates parse-cfg-cli) CFG-CLI->CFG-AST))
  (import (only (euphrates read-list) read-list))
  (import
    (only (tegfs categorization-split)
          categorization-split))
  (import
    (only (scheme base)
          append
          apply
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
          null?
          pair?
          quote))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/categorization-parse.scm")))
    (else (include "categorization-parse.scm"))))
