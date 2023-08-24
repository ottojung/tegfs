
(define-library
  (tegfs web-decode-sentence)
  (export web::decode-sentence)
  (import
    (only (euphrates string-to-words) string->words))
  (import
    (only (tegfs generify-dumped-term)
          generify-dumped-term))
  (import
    (only (tegfs prolog-query-parse)
          prolog-query-parse))
  (import (only (scheme base) begin define))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/web-decode-sentence.scm")))
    (else (include "web-decode-sentence.scm"))))
