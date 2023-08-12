
(define-library
  (tegfs get)
  (export
    tegfs-get
    tegfs-get/parse
    tegfs-get/cached)
  (import
    (only (euphrates absolute-posix-path-q)
          absolute-posix-path?))
  (import (only (euphrates dprintln) dprintln))
  (import
    (only (euphrates hashmap)
          hashmap-clear!
          hashmap-ref
          make-hashmap))
  (import
    (only (tegfs entries-for-each) entries-for-each))
  (import
    (only (tegfs entries-to-hashmap)
          entries->hashmap))
  (import
    (only (tegfs entry-print-formatted)
          entry-print/formatted))
  (import (only (tegfs entry-print) entry-print))
  (import (only (tegfs keyword-id) keyword-id))
  (import (only (tegfs log-info) log-info))
  (import
    (only (tegfs standalone-file-to-entry)
          standalone-file->entry))
  (import
    (only (scheme base)
          assoc
          begin
          call-with-current-continuation
          cdr
          current-error-port
          current-output-port
          define
          equal?
          if
          lambda
          let
          newline
          or
          parameterize
          when))
  (import (only (scheme process-context) exit))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin (include-from-path "tegfs/get.scm")))
    (else (include "get.scm"))))
