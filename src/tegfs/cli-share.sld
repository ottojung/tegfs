
(define-library
  (tegfs cli-share)
  (export CLI::share CLI::share/parse)
  (import (only (euphrates assq-or) assq-or))
  (import
    (only (euphrates catchu-case) catchu-case))
  (import
    (only (euphrates list-and-map) list-and-map))
  (import
    (only (euphrates list-singleton-q)
          list-singleton?))
  (import
    (only (euphrates read-string-file)
          read-string-file))
  (import
    (only (euphrates string-to-seconds)
          string->seconds))
  (import (only (euphrates stringf) stringf))
  (import (only (euphrates tilda-a) ~a))
  (import (only (euphrates un-tilda-s) un~s))
  (import (only (tegfs a-weblink-q) a-weblink?))
  (import (only (tegfs config-get) config-get))
  (import
    (only (tegfs download-string) download-string))
  (import (only (tegfs fatal) fatal))
  (import (only (tegfs get-config) get-config))
  (import
    (only (tegfs keyword-config-fileserver)
          keyword-config-fileserver))
  (import
    (only (tegfs keyword-config-port)
          keyword-config-port))
  (import
    (only (tegfs web-current-fileserver-p)
          web::current-fileserver/p))
  (import
    (only (tegfs web-default-port) web::default-port))
  (import
    (only (tegfs web-get-server-operator-key-file)
          web::get-server-operator-key-file))
  (import
    (only (tegfs web-get-target-link)
          web::get-target-link))
  (import
    (only (scheme base)
          and
          begin
          car
          define
          define-values
          if
          list
          list?
          newline
          null?
          pair?
          parameterize
          quote
          unless
          when))
  (import (only (scheme write) display))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin (include-from-path "tegfs/cli-share.scm")))
    (else (include "cli-share.scm"))))
