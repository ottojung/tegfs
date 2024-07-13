
(define-library
  (compile-parser)
  (import (only (euphrates assert-equal) assert=))
  (import (only (euphrates assert) assert))
  (import
    (only (euphrates current-program-path-p)
          current-program-path/p))
  (import
    (only (euphrates define-cli)
          define-cli:show-help
          with-cli))
  (import
    (only (euphrates irregex) irregex-replace))
  (import (only (euphrates list-last) list-last))
  (import
    (only (euphrates path-get-basename)
          path-get-basename))
  (import
    (only (euphrates path-replace-extension)
          path-replace-extension))
  (import
    (only (euphrates path-without-extension)
          path-without-extension))
  (import (only (euphrates read-list) read-list))
  (import (only (euphrates tilda-a) ~a))
  (import
    (only (parser-compiler-generic)
          parser-compiler/generic))
  (import
    (only (scheme base)
          /
          and
          begin
          cadr
          car
          cond
          current-output-port
          define
          else
          lambda
          let
          list
          list?
          parameterize
          quasiquote
          quote
          string->symbol
          unquote))
  (import
    (only (scheme file)
          call-with-input-file
          call-with-output-file))
  (import (only (scheme process-context) exit))
  (import (only (scheme write) write))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin (include-from-path "compile-parser.scm")))
    (else (include "compile-parser.scm"))))
