
(define-library
  (tegfs web-decode-sentence)
  (export web::decode-sentence)
  (import
    (only (euphrates string-to-words) string->words))
  (import (only (euphrates un-tilda-s) un~s))
  (import
    (only (tegfs prolog-query-parse)
          prolog-query-parse))
  (import
    (only (tegfs prolog-var)
          prolog-var-name
          prolog-var?))
  (import
    (only (scheme base)
          begin
          cdr
          cond
          define
          else
          if
          lambda
          let
          map
          number?
          symbol?))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/web-decode-sentence.scm")))
    (else (include "web-decode-sentence.scm"))))
