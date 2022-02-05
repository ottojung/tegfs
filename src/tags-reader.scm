;;;; Copyright (C) 2022  Otto Jung
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; version 3 of the License.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

%run guile

%var tegfs-categorize/parse
%var tags-reader-edit

%use (make-temporary-filename) "./euphrates/make-temporary-filename.scm"
%use (system-fmt) "./euphrates/system-fmt.scm"
%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (dprintln) "./euphrates/dprintln.scm"
%use (file-or-directory-exists?) "./euphrates/file-or-directory-exists-q.scm"
%use (write-string-file) "./euphrates/write-string-file.scm"
%use (read-string-file) "./euphrates/read-string-file.scm"
%use (comp) "./euphrates/comp.scm"
%use (fn) "./euphrates/fn.scm"
%use (string->lines) "./euphrates/string-to-lines.scm"
%use (string-strip) "./euphrates/string-strip.scm"
%use (string-split/simple) "./euphrates/string-split-simple.scm"
%use (list-split-on) "./euphrates/list-split-on.scm"
%use (define-tuple) "./euphrates/define-tuple.scm"
%use (read-list) "./euphrates/read-list.scm"
%use (lines->string) "./euphrates/lines-to-string.scm"
%use (list-map/flatten) "./euphrates/list-map-flatten.scm"
%use (curry-if) "./euphrates/curry-if.scm"
%use (CFG-CLI->CFG-AST) "./euphrates/parse-cfg-cli.scm"
%use (list-deduplicate) "./euphrates/list-deduplicate.scm"
%use (raisu) "./euphrates/raisu.scm"

%use (categorization-filename) "./categorization-filename.scm"
%use (root/p) "./root-p.scm"

%use (debug) "./euphrates/debug.scm"

(define (tegfs-categorize/parse)
  (define categorization-file (append-posix-path (root/p) categorization-filename))
  (tags-reader-edit categorization-file)
  (dprintln "Edited!"))

(define (tags-reader-edit categorization-file)
  (define working-file (make-temporary-filename))

  (unless (file-or-directory-exists? categorization-file)
    (write-string-file
     categorization-file
     "# This file is for categorization of the tags\n\n-----------\n\n"))

  (copy-file categorization-file working-file)

  (system-fmt "$EDITOR ~a" working-file)

  (process-categorization-text (read-string-file working-file))

  (system-fmt "sed 's/*//g' ~a" working-file)
  (rename-file working-file categorization-file)

  )

(define unstar-symbol
  (comp symbol->string
        string->list
        (filter (negate (comp (equal? #\*))))
        list->string
        string->symbol))

(define type-symbol?
  (comp unstar-symbol
        symbol->string
        (string-suffix? ">")))

(define starred-symbol?
  (comp symbol->string string->list (member #\*)))

(define (get-parents/transitive ast/flatten tag/starred)
  (let loop ((tag/starred tag/starred) (buf '()))
    (when (member tag/starred buf)
      (raisu 'cycle-detected! tag/starred buf))

    (apply
     append
     (filter
      identity
      (map
       (lambda (production)
         (and (member tag/starred (cdr production))
              (let ((immediate (car production)))
                (if (type-symbol? immediate)
                    (loop immediate (cons tag/starred buf))
                    (list immediate)))))
       ast/flatten)))))

(define (has-multiple-parents? ast/flatten/unstarred tag/starred)
  (define tag (unstar-symbol tag/starred))
  (<= 2
      (length
       (filter
        (lambda (production)
          (member tag (cdr production)))
        ast/flatten/unstarred))))

(define (process-categorization-text text)
  (define lines (string->lines text))
  (define stripped (map string-strip lines))
  (define noncommented (map (comp ((fn string-split/simple % #\#)) car) stripped))
  (define-tuple (cfg-part rules-part)
    (map lines->string
         (list-split-on (comp (string-prefix? "----")) noncommented)))

  (define words
    (with-input-from-string cfg-part
      (lambda _
        (read-list (current-input-port)))))

  (define words-flat
    (list-map/flatten (curry-if pair? identity list) words))

  (define ast
    (CFG-CLI->CFG-AST words-flat))

  (debug "parsed: ~s" ast)

  (define ast/flatten
    (map (lambda (production)
           (cons (car production)
                 (list-deduplicate
                  (apply append (cdr production)))))
         ast))

  (debug "parsed/flat: ~s" ast/flatten)

  (define starred
    (filter starred-symbol? words-flat))

  (debug "starred: ~s" starred)

  (define starred-productions
    (filter starred-symbol? (map car ast)))

  (debug "starred-productions: ~s" starred-productions)

  (define starred-non-productions
    (apply
     append
     (map (lambda (production)
            (filter starred-symbol? (cdr production)))
          ast/flatten)))

  (debug "starred-non-productions: ~s" starred-non-productions)

  (define ast/flatten/unstarred
    (map (lambda (production)
           (cons
            (unstar-symbol (car production))
            (list-deduplicate
             (map unstar-symbol (cdr production)))))
         ast/flatten))

  (debug "parsed/flat/unstarred: ~s" ast/flatten/unstarred)

  (define doubles
    (filter (comp (has-multiple-parents? ast/flatten/unstarred)) starred))

  (debug "doubles: ~s" doubles)

  (define doubles-parents
    (map unstar-symbol (apply append (map (comp (get-parents/transitive ast/flatten)) doubles))))

  (debug "doubles-parents: ~s" doubles-parents)

  (define all-starred-productions
    (append starred-productions doubles-parents))

  (debug "all-starred-productions: ~s" all-starred-productions)

  (define ambiguous-starred-productions
    (let ((starred-non-productions/unstarred
           (map unstar-symbol starred-non-productions)))
      (filter
       (lambda (tag/starred)
         (define tag (unstar-symbol tag/starred))
         (and (has-multiple-parents? ast/flatten/unstarred tag)
              (not (member tag starred-non-productions/unstarred))))
       all-starred-productions)))

  (debug "ambiguous-starred-productions: ~s" ambiguous-starred-productions)

  (if (null? ambiguous-starred-productions)
      (cons
       'ok
       (list-deduplicate
        (map unstar-symbol (append starred doubles-parents))))
      (cons
       'error
       (map
        (lambda (amb)
          (cons (unstar-symbol amb)
                (map unstar-symbol (get-parents/transitive ast/flatten amb))))
        ambiguous-starred-productions))))



