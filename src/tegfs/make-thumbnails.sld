
(define-library
  (tegfs make-thumbnails)
  (export
    tegfs-make-thumbnails/parse
    tegfs-make-thumbnails
    tegfs-make-image-thumbnails
    tegfs-make-senderideo-thumbnails)
  (import
    (only (euphrates asyncproc-input-text-p)
          asyncproc-input-text/p))
  (import
    (only (euphrates catchu-case) catchu-case))
  (import (only (euphrates comp) appcomp comp))
  (import
    (only (euphrates file-delete) file-delete))
  (import
    (only (euphrates file-or-directory-exists-q)
          file-or-directory-exists?))
  (import
    (only (euphrates list-ref-or) list-ref-or))
  (import
    (only (euphrates make-directories)
          make-directories))
  (import
    (only (euphrates make-temporary-filename)
          make-temporary-filename))
  (import
    (only (euphrates path-get-dirname)
          path-get-dirname))
  (import (only (euphrates raisu) raisu))
  (import
    (only (euphrates run-syncproc-re-star)
          run-syncproc/re*))
  (import
    (only (euphrates run-syncproc) run-syncproc))
  (import
    (only (euphrates string-strip) string-strip))
  (import
    (only (euphrates string-to-lines) string->lines))
  (import
    (only (euphrates string-to-seconds-columned)
          string->seconds/columned))
  (import
    (only (euphrates string-to-words) string->words))
  (import (only (euphrates stringf) stringf))
  (import (only (euphrates system-fmt) system-fmt))
  (import
    (only (euphrates system-star-exit-code)
          system*/exit-code))
  (import (only (euphrates tilda-a) ~a))
  (import (only (euphrates url-goto) url-goto))
  (import
    (only (euphrates with-output-stringified)
          with-output-stringified))
  (import (only (tegfs a-weblink-q) a-weblink?))
  (import (only (tegfs fatal) fatal))
  (import
    (only (tegfs file-is-image-q) file-is-image?))
  (import
    (only (tegfs file-is-video-q) file-is-video?))
  (import (only (tegfs log-info) log-info))
  (import (only (tegfs log-warning) log-warning))
  (import
    (only (tegfs make-temporary-filename-local)
          make-temporary-filename/local))
  (import
    (only (tegfs web-preview-height)
          web::preview-height))
  (import
    (only (tegfs web-preview-width)
          web::preview-width))
  (import
    (only (scheme base)
          *
          /
          =
          begin
          cadr
          car
          ceiling
          cond
          current-error-port
          current-output-port
          define
          define-values
          do
          else
          lambda
          let
          let*
          map
          or
          parameterize
          quote
          set!
          string-append
          unless
          when))
  (import (only (scheme r5rs) inexact->exact))
  (cond-expand
    (guile (import (only (srfi srfi-1) filter)))
    (else (import (only (srfi 1) filter))))
  (cond-expand
    (guile (import
             (only (srfi srfi-13) string-null? string-prefix?)))
    (else (import
            (only (srfi 13) string-null? string-prefix?))))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "tegfs/make-thumbnails.scm")))
    (else (include "make-thumbnails.scm"))))
