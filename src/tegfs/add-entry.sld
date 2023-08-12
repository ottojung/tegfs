
(define-library
  (tegfs add-entry)
  (export add-entry)
  (import
    (only (euphrates absolute-posix-path-q)
          absolute-posix-path?))
  (import
    (only (euphrates alphanum-lowercase-alphabet)
          alphanum-lowercase/alphabet))
  (import
    (only (euphrates append-posix-path)
          append-posix-path))
  (import
    (only (euphrates append-string-file)
          append-string-file))
  (import
    (only (euphrates assoc-set-default)
          assoc-set-default))
  (import
    (only (euphrates assoc-set-value)
          assoc-set-value))
  (import (only (euphrates assq-or) assq-or))
  (import
    (only (euphrates file-or-directory-exists-q)
          file-or-directory-exists?))
  (import
    (only (euphrates list-deduplicate)
          list-deduplicate/reverse))
  (import
    (only (euphrates make-directories)
          make-directories))
  (import (only (euphrates raisu) raisu))
  (import
    (only (euphrates random-choice) random-choice))
  (import
    (only (euphrates read-string-file)
          read-string-file))
  (import
    (only (euphrates string-strip) string-strip))
  (import (only (euphrates tilda-a) ~a))
  (import
    (only (euphrates with-output-stringified)
          with-output-stringified))
  (import
    (only (euphrates write-string-file)
          write-string-file))
  (import (only (tegfs a-weblink-q) a-weblink?))
  (import
    (only (tegfs default-db-path) default-db-path))
  (import (only (tegfs entry-get-id) entry-get-id))
  (import
    (only (tegfs entry-get-target) entry-get-target))
  (import (only (tegfs entry-print) entry-print))
  (import
    (only (tegfs generate-entry-date-timestamp)
          generate-entry-date-timestamp))
  (import
    (only (tegfs get-file-mimetype)
          get-file-mimetype))
  (import
    (only (tegfs get-registry-files)
          get-registry-files))
  (import (only (tegfs get-root) get-root))
  (import (only (tegfs get) tegfs-get))
  (import (only (tegfs keyword-date) keyword-date))
  (import (only (tegfs keyword-id) keyword-id))
  (import
    (only (tegfs keyword-mimetype) keyword-mimetype))
  (import (only (tegfs keyword-prev) keyword-prev))
  (import (only (tegfs keyword-tags) keyword-tags))
  (import
    (only (tegfs last-id-filename) last-id-filename))
  (import
    (only (scheme base)
          and
          assq
          begin
          car
          cond
          define
          else
          equal?
          if
          let
          list->string
          map
          newline
          not
          or
          quote
          string->symbol
          symbol?
          unless
          when))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin (include-from-path "tegfs/add-entry.scm")))
    (else (include "add-entry.scm"))))
