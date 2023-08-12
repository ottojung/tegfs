
(define-library
  (tegfs CLI-read-answer-string)
  (export CLI::read-answer-string)
  (import
    (only (euphrates read-string-line)
          read-string-line))
  (import
    (only (tegfs CLI-string-index-to-key)
          CLI::string-index->key))
  (import
    (only (tegfs CLI-swiched-field-huh-p)
          CLI::swiched-field?/p))
  (import (only (tegfs fatal) fatal))
  (import (only (tegfs log-error) log-error))
  (import (only (tegfs log-question) log-question))
  (import
    (only (scheme base)
          begin
          cdr
          define
          eof-object?
          if
          let
          parameterize
          quote
          string->number
          string-append
          when))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/CLI-read-answer-string.scm")))
    (else (include "CLI-read-answer-string.scm"))))
