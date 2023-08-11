
(define-library
  (tegfs tegfs-query-noopen)
  (export tegfs-query/noopen)
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
  (import
    (only (euphrates profun-database)
          profun-database-extend))
  (import
    (only (euphrates profun-standard-handler)
          profun-standard-handler))
  (import
    (only (euphrates profun)
          profun-create-falsy-database
          profun-eval-query/boolean))
  (import (only (euphrates raisu) raisu))
  (import
    (only (euphrates run-syncproc-re-star)
          run-syncproc/re*))
  (import
    (only (euphrates stack)
          stack->list
          stack-make
          stack-push!))
  (import
    (only (euphrates string-strip) string-strip))
  (import
    (only (euphrates string-to-lines) string->lines))
  (import (only (euphrates tilda-a) ~a))
  (import (only (tegfs dump-rules) dump-rules))
  (import
    (only (tegfs entries-iterate) entries-iterate))
  (import (only (tegfs keyword-id) keyword-id))
  (import
    (only (tegfs make-temporary-filename-local)
          make-temporary-filename/local))
  (import
    (only (tegfs prolog-query-parse)
          prolog-query-parse))
  (import
    (only (tegfs prolog-var)
          make-prolog-var
          prolog-var-name
          prolog-var?))
  (import
    (only (tegfs prolog)
          tegfs-dump-prolog
          translate-entry-tags))
  (import
    (only (tegfs tag-to-prolog-term)
          tag->prolog-term))
  (import
    (only (scheme base)
          and
          append
          apply
          begin
          cadr
          cddr
          cdr
          close-port
          cond
          current-output-port
          define
          define-values
          else
          equal?
          if
          lambda
          let
          list
          map
          newline
          not
          null?
          parameterize
          quasiquote
          quote
          string->symbol
          string-append
          symbol?
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
               "tegfs/tegfs-query-noopen.scm")))
    (else (include "tegfs-query-noopen.scm"))))