
(define-library
  (tegfs web-collectgarbage-nocall)
  (export web::collectgarbage/nocall)
  (import
    (only (euphrates directory-files)
          directory-files))
  (import
    (only (euphrates file-delete) file-delete))
  (import
    (only (euphrates hashmap)
          hashmap-delete!
          hashmap-foreach))
  (import
    (only (euphrates path-without-extension)
          path-without-extension))
  (import (only (euphrates raisu) raisu))
  (import
    (only (tegfs current-time-p) current-time/p))
  (import
    (only (tegfs custom-tempentry-still-valid-huh)
          custom-tempentry-still-valid?))
  (import
    (only (tegfs custom-tempentry) custom-tempentry?))
  (import
    (only (tegfs filemap)
          filemap-delete-by-recepientid!
          filemap-ref-by-recepientid))
  (import (only (tegfs log-info) log-info))
  (import (only (tegfs log-warning) log-warning))
  (import
    (only (tegfs permission-still-valid-huh)
          permission-still-valid?))
  (import
    (only (tegfs permission)
          permission-filemap
          permission?))
  (import
    (only (tegfs sharedinfo-still-valid-huh)
          sharedinfo-still-valid?))
  (import
    (only (tegfs sharedinfo)
          sharedinfo-recepientid
          sharedinfo?))
  (import
    (only (tegfs sharereceipt) sharereceipt?))
  (import
    (only (tegfs webcore-context)
          context-filemap/2
          context-sharedir
          context-tempentries))
  (import
    (only (scheme base)
          _
          begin
          cadr
          car
          cond
          cons
          define
          define-syntax
          else
          for-each
          if
          lambda
          let
          let*
          or
          quote
          set!
          syntax-rules
          unless))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path
               "tegfs/web-collectgarbage-nocall.scm")))
    (else (include "web-collectgarbage-nocall.scm"))))
