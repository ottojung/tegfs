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
%var tegfs-categorize
%var tegfs-edit-tags

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
%use (file-delete) "./euphrates/file-delete.scm"
%use (list-and-map) "./euphrates/list-and-map.scm"
%use (compose-under) "./euphrates/compose-under.scm"
%use (read-string-line) "./euphrates/read-string-line.scm"

%use (categorization-filename) "./categorization-filename.scm"
%use (root/p) "./root-p.scm"

(define (tegfs-categorize/parse)
  (define result (tegfs-categorize #f))
  (dprintln "Categorized! Chosen tags: ~s" (cdr result)))

(define (tegfs-categorize working-file-maybe)
  (define categorization-file (append-posix-path (root/p) categorization-filename))
  (define working-file
    (or working-file-maybe
        (make-temporary-filename)))

  (unless (file-or-directory-exists? categorization-file)
    (write-string-file
     categorization-file
     "# This file is for categorization of the tags\n\n-----------\n\n"))

  (unless (file-or-directory-exists? working-file)
    (copy-file categorization-file working-file))

  (let loop ()
    (define result (tegfs-edit-tags working-file))
    (if (equal? 'ok (car result))
        (begin
          (if working-file-maybe
              (copy-file working-file categorization-file)
              (rename-file working-file categorization-file))
          (system-fmt "sed -i 's/\\*//g ; s/\\w\\w*_\\(\\w\\w*\\)/_\\1/g ; s/\\(\\w\\w*\\)^\\w\\w*/\\1^/g' ~a"
                      categorization-file)
          result)
        (begin
          (dprintln "Error categorizing:")
          (print-errors (cdr result))
          (dprintln "Press enter to continue...")
          (read-string-line)
          (loop)))))

(define (print-errors errors)
  (for-each
   (lambda (line)
     (dprintln "Tag \"~s\" has ambiguous parents: ~s" (car line) (cdr line)))
   errors))

(define (tegfs-edit-tags working-file)
  (unless working-file
    (raisu 'must-provide-working-file))

  (system-fmt "$EDITOR ~a" working-file)
  (process-categorization-text (read-string-file working-file)))

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

;; transitive over type-symbols, but not a general transitive closure
(define (get-parents ast/flatten tag/starred)
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

;; transitive over symbols that have single parent
(define (get-parents/transitive/reflexive ast/flatten tag/starred)
  (cons
   tag/starred
   (let loop ((tag/starred tag/starred) (buf '()))
     (when (member tag/starred buf)
       (raisu 'cycle-detected! tag/starred buf))

     (let ((immediate-parents
            (filter
             identity
             (map
              (lambda (production)
                (and (member tag/starred (cdr production))
                     (car production)))
              ast/flatten))))
       (if (or (null? immediate-parents)
               (not (null? (cdr immediate-parents))))
           '()
           (let ((parent (car immediate-parents)))
             (cons parent (loop parent (cons tag/starred buf)))))))))

;; returns either `(ok ,list-of-chosen-tags)
;;             or `(error ,list-of-ambiguous-tags-with-parents)
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

  (define ast/flatten
    (map (lambda (production)
           (cons (car production)
                 (list-deduplicate
                  (apply append (cdr production)))))
         ast))

  (define starred
    (filter starred-symbol? words-flat))

  (define starred/unstarred
    (map unstar-symbol starred))

  (define starred-productions
    (filter starred-symbol? (map car ast)))

  (define starred-non-productions
    (apply
     append
     (map (lambda (production)
            (filter starred-symbol? (cdr production)))
          ast/flatten)))

  (define ast/flatten/unstarred
    (map (lambda (production)
           (cons
            (unstar-symbol (car production))
            (list-deduplicate
             (map unstar-symbol (cdr production)))))
         ast/flatten))

  (define doubles
    (filter
     (comp unstar-symbol (get-parents ast/flatten/unstarred) length (< 1))
     starred))

  (define doubles-parents
    (apply append (map (comp (get-parents ast/flatten)) doubles)))

  (define all-parents
    (list-deduplicate
     (append
      starred-productions
      (apply
       append
       (map (comp (get-parents ast/flatten)) starred-non-productions)))))

  (define tags-to-return/starred
    (list-deduplicate (append starred doubles-parents)))

  (define tags-to-return
    (map unstar-symbol tags-to-return/starred))

  (define all-parents/transitive/reflexive
    (map
     unstar-symbol
     (list-deduplicate
      (apply
       append
       (map (comp (get-parents/transitive/reflexive ast/flatten)) tags-to-return/starred)))))

  (define ambiguous-starred-productions
    (let ((starred-non-productions/unstarred
           (map unstar-symbol starred-non-productions)))
      (filter
       (lambda (tag/starred)
         (define tag (unstar-symbol tag/starred))
         (define parents (get-parents ast/flatten/unstarred tag))
         (and (< 1 (length parents))
              (list-and-map
               (fn not (member % all-parents/transitive/reflexive))
               parents)))
       all-parents)))

  (if (null? ambiguous-starred-productions)
      (cons 'ok tags-to-return)
      (cons
       'error
       (map
        (lambda (amb)
          (define unstarred (unstar-symbol amb))
          (cons unstarred
                (map unstar-symbol (get-parents ast/flatten/unstarred unstarred))))
        ambiguous-starred-productions))))



