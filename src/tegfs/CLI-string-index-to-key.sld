
(define-library
  (tegfs CLI-string-index-to-key)
  (export CLI::string-index->key)
  (import
    (only (euphrates alist-initialize-bang)
          alist-initialize!:current-setters))
  (import
    (only (scheme base)
          -
          <
          =
          begin
          car
          cdr
          define
          if
          let
          null?))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/CLI-string-index-to-key.scm")))
    (else (include "CLI-string-index-to-key.scm"))))
