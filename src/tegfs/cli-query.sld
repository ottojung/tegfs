
(define-library
  (tegfs cli-query)
  (export CLI::query)
  (import (only (euphrates comp) comp))
  (import (only (euphrates fn-alist) fn-alist))
  (import
    (only (euphrates profune-communicator)
          profune-communicator-handle))
  (import
    (only (tegfs entry-print-formatted)
          entry-print/formatted))
  (import (only (tegfs entry-print) entry-print))
  (import (only (tegfs fatal) fatal))
  (import (only (tegfs log-info) log-info))
  (import
    (only (tegfs tegfs-make-communicator)
          tegfs-make-communicator))
  (import
    (only (scheme base)
          begin
          cadr
          cond
          define
          else
          equal?
          for-each
          if
          lambda
          length
          let
          newline
          or
          quasiquote
          quote
          unquote))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin (include-from-path "tegfs/cli-query.scm")))
    (else (include "cli-query.scm"))))
