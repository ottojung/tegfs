
(define-library
  (tegfs prolog)
  (export
    tegfs-dump-prolog
    tegfs-dump-prolog-file
    translate-entry-tags)
  (import (only (euphrates assq-or) assq-or))
  (import (only (euphrates comp) appcomp))
  (import
    (only (euphrates hashset)
          hashset->list
          hashset-add!
          make-hashset))
  (import
    (only (euphrates list-deduplicate)
          list-deduplicate/reverse))
  (import
    (only (euphrates open-file-port) open-file-port))
  (import (only (euphrates raisu) raisu))
  (import
    (only (euphrates stack)
          stack->list
          stack-make
          stack-push!))
  (import (only (euphrates tilda-a) ~a))
  (import (only (tegfs a-weblink-q) a-weblink?))
  (import (only (tegfs dump-rules) dump-rules))
  (import
    (only (tegfs entries-for-each) entries-for-each))
  (import
    (only (tegfs entry-get-target) entry-get-target))
  (import (only (tegfs keyword-id) keyword-id))
  (import (only (tegfs keyword-tags) keyword-tags))
  (import
    (only (tegfs make-temporary-filename-local)
          make-temporary-filename/local))
  (import
    (only (tegfs print-prolog-inference)
          print-prolog-inference))
  (import
    (only (tegfs prolog-additional-static-predicates)
          prolog-additional-static-predicates))
  (import
    (only (tegfs tag-to-prolog-term)
          print-tag-as-prolog-term))
  (import
    (only (tegfs tags-this-variable)
          tags-this-variable/string))
  (import
    (only (scheme base)
          +
          <
          _
          append
          apply
          begin
          car
          case
          cdr
          close-port
          cons
          current-output-port
          define
          equal?
          for-each
          if
          lambda
          let
          map
          null?
          number->string
          parameterize
          quasiquote
          quote
          set!
          string-append
          unless
          unquote
          unquote-splicing
          when))
  (import (only (scheme write) display))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin (include-from-path "tegfs/prolog.scm")))
    (else (include "prolog.scm"))))
