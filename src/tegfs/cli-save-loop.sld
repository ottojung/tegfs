
(define-library
  (tegfs cli-save-loop)
  (export CLI::save::loop)
  (import
    (only (euphrates alist-initialize-bang)
          alist-initialize!:current-setters
          alist-initialize!:return-multiple
          alist-initialize!:unset))
  (import
    (only (euphrates alist-initialize-loop)
          alist-initialize-loop))
  (import
    (only (euphrates append-posix-path)
          append-posix-path))
  (import (only (euphrates assq-or) assq-or))
  (import (only (euphrates dprintln) dprintln))
  (import
    (only (euphrates list-take-n) list-take-n))
  (import
    (only (euphrates path-extensions)
          path-extensions))
  (import
    (only (euphrates path-get-basename)
          path-get-basename))
  (import
    (only (euphrates path-without-extension)
          path-without-extension))
  (import
    (only (euphrates print-in-frame) print-in-frame))
  (import (only (euphrates range) range))
  (import
    (only (euphrates read-string-file)
          read-string-file))
  (import
    (only (euphrates string-to-words) string->words))
  (import (only (euphrates tilda-s) ~s))
  (import
    (only (euphrates url-get-path) url-get-path))
  (import
    (only (euphrates words-to-string) words->string))
  (import
    (only (euphrates write-string-file)
          write-string-file))
  (import
    (only (tegfs CLI-read-answer-string)
          CLI::read-answer-string))
  (import
    (only (tegfs CLI-read-enumeration)
          CLI::read-enumeration))
  (import (only (tegfs a-weblink-q) a-weblink?))
  (import
    (only (tegfs categorization-complete-selection)
          categorization-complete-selection))
  (import
    (only (tegfs categorization-filename)
          categorization-filename))
  (import
    (only (tegfs categorize) tegfs-categorize))
  (import
    (only (tegfs cli-save-working-file-p)
          CLI::save-working-file/p))
  (import
    (only (tegfs clipboard)
          classify-clipboard-text-content
          dump-clipboard-to-temporary
          get-clipboard-text-content
          get-clipboard-type-extension))
  (import
    (only (tegfs download-to-temporary-file)
          download-to-temporary-file))
  (import (only (tegfs fatal) fatal))
  (import
    (only (tegfs file-is-audio-q) file-is-audio?))
  (import
    (only (tegfs file-is-image-q) file-is-image?))
  (import
    (only (tegfs file-is-video-q) file-is-video?))
  (import
    (only (tegfs get-file-mimetype)
          get-file-mimetype))
  (import
    (only (tegfs get-random-basename)
          get-random-basename))
  (import (only (tegfs get-root) get-root))
  (import
    (only (tegfs get-save-plugins) get-save-plugins))
  (import (only (tegfs root-p) root/p))
  (import
    (only (tegfs run-save-plugins) run-save-plugins))
  (import
    (only (tegfs tag-choice-to-immediate-tag)
          tag-choice->immediate-tag))
  (import
    (only (scheme base)
          *
          +
          -
          <
          =
          and
          begin
          car
          cdr
          cond
          cons
          define
          else
          equal?
          for-each
          if
          lambda
          length
          let
          let*
          list->string
          list?
          make-parameter
          map
          member
          newline
          not
          null?
          or
          quasiquote
          quote
          string->list
          string->symbol
          string-append
          string-length
          string?
          unless
          unquote))
  (cond-expand
    (guile (import (only (srfi srfi-1) filter)))
    (else (import (only (srfi 1) filter))))
  (cond-expand
    (guile (import (only (srfi srfi-13) string-prefix?)))
    (else (import (only (srfi 13) string-prefix?))))
  (cond-expand
    (guile (import (only (srfi srfi-17) setter)))
    (else (import (only (srfi 17) setter))))
  (cond-expand
    (guile (import (only (guile) include-from-path))
           (begin
             (include-from-path "tegfs/cli-save-loop.scm")))
    (else (include "cli-save-loop.scm"))))
