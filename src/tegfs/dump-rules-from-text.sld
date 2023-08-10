
(define-library
  (tegfs dump-rules-from-text)
  (export dump-rules-from-text)
  (import (only (euphrates comp) comp))
  (import
    (only (euphrates define-tuple) define-tuple))
  (import
    (only (euphrates list-split-on) list-split-on))
  (import (only (euphrates raisu) raisu))
  (import
    (only (euphrates string-split-3) string-split-3))
  (import
    (only (euphrates string-to-lines) string->lines))
  (import
    (only (euphrates string-to-words) string->words))
  (import (only (tegfs fatal) fatal))
  (import
    (only (tegfs make-term-parser) make-term-parser))
  (import
    (only (tegfs prolog-cut-symbol)
          make-prolog-cut-symbol))
  (import
    (only (tegfs prolog-query-parse)
          prolog-query-parse))
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
          equal?
          for-each
          lambda
          length
          list
          null?
          quote
          string->symbol
          unless))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/dump-rules-from-text.scm")))
    (else (include "dump-rules-from-text.scm"))))
