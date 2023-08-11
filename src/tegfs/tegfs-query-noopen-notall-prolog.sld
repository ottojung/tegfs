
(define-library
  (tegfs tegfs-query-noopen-notall-prolog)
  (export tegfs-query/noopen/notall/prolog)
  (import (only (euphrates assq-or) assq-or))
  (import (only (euphrates comp) appcomp))
  (import
    (only (euphrates file-delete) file-delete))
  (import
    (only (euphrates hashset)
          hashset-has?
          make-hashset))
  (import
    (only (euphrates list-intersperse)
          list-intersperse))
  (import (only (euphrates negate) negate))
  (import
    (only (euphrates open-file-port) open-file-port))
  (import (only (euphrates printf) printf))
  (import (only (euphrates raisu) raisu))
  (import
    (only (euphrates run-syncproc-re-star)
          run-syncproc/re*))
  (import
    (only (euphrates string-strip) string-strip))
  (import
    (only (euphrates string-to-lines) string->lines))
  (import (only (tegfs keyword-id) keyword-id))
  (import
    (only (tegfs make-temporary-filename-local)
          make-temporary-filename/local))
  (import
    (only (tegfs prolog-query-parse)
          prolog-query-parse))
  (import
    (only (tegfs prolog-var) make-prolog-var))
  (import (only (tegfs prolog) tegfs-dump-prolog))
  (import
    (only (tegfs tag-to-prolog-term)
          tag->prolog-term))
  (import
    (only (scheme base)
          and
          append
          apply
          begin
          close-port
          current-output-port
          define
          define-values
          equal?
          if
          lambda
          let
          map
          newline
          not
          null?
          parameterize
          quasiquote
          quote
          string-append
          unless
          unquote))
  (import (only (scheme write) display))
  (cond-expand
    (guile (import (only (srfi srfi-1) filter)))
    (else (import (only (srfi 1) filter))))
  (cond-expand
    (guile (import (only (srfi srfi-13) string-null?)))
    (else (import (only (srfi 13) string-null?))))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/tegfs-query-noopen-notall-prolog.scm")))
    (else (include "tegfs-query-noopen-notall-prolog.scm"))))
