
(define-library
  (tegfs categorize)
  (export tegfs-categorize/parse tegfs-categorize)
  (import
    (only (euphrates alist-initialize)
          alist-initialize))
  (import
    (only (euphrates append-posix-path)
          append-posix-path))
  (import (only (euphrates assq-or) assq-or))
  (import (only (euphrates comp) comp))
  (import (only (euphrates const) const))
  (import (only (euphrates curry-if) curry-if))
  (import
    (only (euphrates file-or-directory-exists-q)
          file-or-directory-exists?))
  (import
    (only (euphrates hashmap)
          hashmap-ref
          hashmap-set!
          make-hashmap))
  (import
    (only (euphrates hashset)
          hashset->list
          hashset-add!
          hashset-difference
          make-hashset))
  (import
    (only (euphrates lines-to-string) lines->string))
  (import (only (euphrates negate) negate))
  (import (only (euphrates raisu) raisu))
  (import
    (only (euphrates read-string-file)
          read-string-file))
  (import
    (only (euphrates read-string-line)
          read-string-line))
  (import (only (euphrates stringf) stringf))
  (import (only (euphrates system-fmt) system-fmt))
  (import (only (euphrates tilda-a) ~a))
  (import
    (only (euphrates words-to-string) words->string))
  (import
    (only (euphrates write-string-file)
          write-string-file))
  (import
    (only (tegfs categorization-filename)
          categorization-filename))
  (import
    (only (tegfs categorization-read)
          categorization-read))
  (import
    (only (tegfs categorization-split)
          categorization-split))
  (import (only (tegfs edit-tags) tegfs-edit-tags))
  (import (only (tegfs get-root) get-root))
  (import (only (tegfs log-error) log-error))
  (import (only (tegfs log-info) log-info))
  (import (only (tegfs log-question) log-question))
  (import
    (only (tegfs make-tag-parser) make-tag-parser))
  (import
    (only (tegfs make-temporary-filename-local)
          make-temporary-filename/local))
  (import
    (only (tegfs tags-this-variable)
          tags-this-variable
          tags-this-variable/string))
  (import (only (tegfs unparse-tag) unparse-tag))
  (import
    (only (scheme base)
          +
          _
          append
          apply
          assoc
          begin
          call-with-values
          car
          cdr
          cond
          define
          do
          else
          equal?
          for-each
          if
          lambda
          let
          let*
          list
          map
          member
          null?
          or
          quote
          set!
          string-append
          unless
          when))
  (import (only (scheme file) delete-file))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "tegfs/categorize.scm")))
    (else (include "categorize.scm"))))
