
(define-library
  (tegfs CLI-read-enumeration)
  (export CLI::read-enumeration)
  (import
    (only (euphrates list-intersperse)
          list-intersperse))
  (import (only (euphrates stringf) stringf))
  (import (only (euphrates tilda-a) ~a))
  (import
    (only (tegfs CLI-read-answer-string)
          CLI::read-answer-string))
  (import (only (tegfs log-question) log-question))
  (import
    (only (scheme base)
          apply
          begin
          define
          if
          let
          map
          member
          string->symbol
          string-append))
  (import (only (scheme char) string-downcase))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/CLI-read-enumeration.scm")))
    (else (include "CLI-read-enumeration.scm"))))
