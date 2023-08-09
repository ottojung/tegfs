
(define-library
  (tegfs CLI-read-answer-string)
  (export CLI::read-answer-string)
  (import (only (euphrates dprintln) dprintln))
  (import
    (only (euphrates read-string-line)
          read-string-line))
  (import
    (only (tegfs CLI-string-index-to-key)
          CLI::string-index->key))
  (import
    (only (scheme base)
          begin
          cdr
          define
          if
          let
          parameterize
          quote
          string->number))
  (import (only (scheme write) display))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/CLI-read-answer-string.scm")))
    (else (include "CLI-read-answer-string.scm"))))
