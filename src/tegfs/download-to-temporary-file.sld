
(define-library
  (tegfs download-to-temporary-file)
  (export download-to-temporary-file)
  (import (only (euphrates tilda-s) ~s))
  (import
    (only (euphrates url-get-hostname-and-port)
          url-get-hostname-and-port))
  (import
    (only (euphrates url-get-protocol)
          url-get-protocol))
  (import
    (only (tegfs download-file) download-file))
  (import (only (tegfs fatal) fatal))
  (import (only (tegfs log-info) log-info))
  (import
    (only (tegfs make-temporary-filename-local)
          make-temporary-filename/local))
  (import
    (only (scheme base)
          =
          begin
          define
          let*
          quasiquote
          quote
          string-append
          unless
          unquote))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/download-to-temporary-file.scm")))
    (else (include "download-to-temporary-file.scm"))))
