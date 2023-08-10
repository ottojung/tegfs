
(define-library
  (tegfs dump-rules)
  (export dump-rules)
  (import
    (only (euphrates append-posix-path)
          append-posix-path))
  (import (only (euphrates comp) comp))
  (import
    (only (euphrates define-tuple) define-tuple))
  (import
    (only (euphrates file-or-directory-exists-q)
          file-or-directory-exists?))
  (import
    (only (euphrates list-split-on) list-split-on))
  (import
    (only (euphrates open-file-port) open-file-port))
  (import (only (euphrates raisu) raisu))
  (import
    (only (euphrates read-string-line)
          read-string-line))
  (import
    (only (euphrates string-split-3) string-split-3))
  (import
    (only (euphrates string-to-words) string->words))
  (import
    (only (euphrates write-string-file)
          write-string-file))
  (import (only (tegfs fatal) fatal))
  (import (only (tegfs get-root) get-root))
  (import
    (only (tegfs make-term-parser) make-term-parser))
  (import
    (only (tegfs prolog-cut-symbol)
          make-prolog-cut-symbol))
  (import
    (only (tegfs prolog-query-parse)
          prolog-query-parse))
  (import
    (only (tegfs rules-filename) rules-filename))
  (import
    (only (tegfs tags-this-variable)
          tags-this-variable/string))
  (import
    (only (scheme base)
          -
          <
          begin
          car
          case
          cdr
          cons
          define
          define-values
          else
          eof-object?
          equal?
          for-each
          if
          lambda
          length
          let
          list
          null?
          quote
          reverse
          string->symbol
          unless))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "tegfs/dump-rules.scm")))
    (else (include "dump-rules.scm"))))
