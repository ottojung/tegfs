
(define-library
  (parser-compiler-generic)
  (export parser-compiler/generic)
  (import
    (only (euphrates parselynn-simple-deserialize-lists)
          parselynn:simple:deserialize/lists))
  (import
    (only (euphrates parselynn-simple-serialize)
          parselynn:simple:serialize))
  (import (only (euphrates stringf) stringf))
  (import
    (only (scheme base)
          _
          begin
          current-output-port
          define
          define-syntax
          lambda
          parameterize
          quasiquote
          string->symbol
          syntax-rules
          unquote))
  (import
    (only (scheme file) call-with-output-file))
  (import (only (scheme write) write))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "parser-compiler-generic.scm")))
    (else (include "parser-compiler-generic.scm"))))
