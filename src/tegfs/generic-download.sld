
(define-library
  (tegfs generic-download)
  (export generic-download)
  (import
    (only (euphrates list-map-flatten)
          list-map/flatten))
  (import (only (euphrates raisu) raisu))
  (import
    (only (euphrates run-syncproc-re-star)
          run-syncproc/re*))
  (import
    (only (euphrates system-star-exit-code)
          system*/exit-code))
  (import (only (euphrates tilda-a) ~a))
  (import
    (only (tegfs verbosity-level-p)
          verbosity-level/p))
  (import
    (only (scheme base)
          >
          append
          apply
          begin
          car
          cdr
          cond
          define
          else
          equal?
          lambda
          list
          quote
          string
          string-append
          string?
          values))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "tegfs/generic-download.scm")))
    (else (include "generic-download.scm"))))
