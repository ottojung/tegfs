
(define-library
  (tegfs cli-save)
  (export CLI::save)
  (import
    (only (euphrates append-posix-path)
          append-posix-path))
  (import (only (euphrates assq-or) assq-or))
  (import
    (only (euphrates file-delete) file-delete))
  (import
    (only (euphrates file-or-directory-exists-q)
          file-or-directory-exists?))
  (import
    (only (euphrates get-command-line-arguments)
          get-command-line-arguments))
  (import
    (only (euphrates make-directories)
          make-directories))
  (import
    (only (euphrates path-get-basename)
          path-get-basename))
  (import
    (only (euphrates path-normalize) path-normalize))
  (import (only (euphrates raisu) raisu))
  (import
    (only (euphrates run-syncproc) run-syncproc))
  (import (only (euphrates stringf) stringf))
  (import
    (only (tegfs add) tegfs-add tegfs-add-file))
  (import (only (tegfs cli-remote) CLI::remote))
  (import
    (only (tegfs cli-save-loop) CLI::save::loop))
  (import
    (only (tegfs cli-save-working-file-p)
          CLI::save-working-file/p))
  (import (only (tegfs cli-share) CLI::share))
  (import
    (only (tegfs clipboard)
          classify-clipboard-text-content))
  (import
    (only (tegfs default-db-path) default-db-path))
  (import
    (only (tegfs dump-clipboard)
          tegfs-dump-clipboard
          tegfs-dump-clipboard/pasta))
  (import (only (tegfs fatal) fatal))
  (import
    (only (tegfs get-random-basename)
          get-random-basename))
  (import (only (tegfs get-root) get-root))
  (import (only (tegfs keyword-id) keyword-id))
  (import
    (only (tegfs make-temporary-filename-local)
          make-temporary-filename/local))
  (import
    (only (scheme base)
          =
          and
          append
          assoc
          begin
          case
          cdr
          cond
          cons
          define
          else
          equal?
          if
          let
          list
          newline
          or
          parameterize
          quote
          string-append
          string?
          unless))
  (import (only (scheme write) display))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin (include-from-path "tegfs/cli-save.scm")))
    (else (include "cli-save.scm"))))
