
(define-library
  (tegfs download-string)
  (export download-string)
  (import (only (euphrates raisu) raisu))
  (import
    (only (tegfs generic-download) generic-download))
  (import
    (only (scheme base)
          =
          begin
          define
          define-values
          quote
          string
          unless))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "tegfs/download-string.scm")))
    (else (include "download-string.scm"))))
