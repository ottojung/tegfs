
(define-library
  (tegfs download-file)
  (export download-file)
  (import (only (euphrates raisu) raisu))
  (import
    (only (tegfs generic-download) generic-download))
  (import
    (only (scheme base)
          begin
          define
          define-values
          quote
          string?
          unless))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "tegfs/download-file.scm")))
    (else (include "download-file.scm"))))
