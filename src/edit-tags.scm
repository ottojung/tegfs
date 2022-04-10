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

%var tegfs-edit-tags
%var tegfs-process-categorization-text

%use (make-temporary-filename) "./euphrates/make-temporary-filename.scm"
%use (system-fmt) "./euphrates/system-fmt.scm"
%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (dprintln) "./euphrates/dprintln.scm"
%use (file-or-directory-exists?) "./euphrates/file-or-directory-exists-q.scm"
%use (write-string-file) "./euphrates/write-string-file.scm"
%use (read-string-file) "./euphrates/read-string-file.scm"
%use (appcomp comp) "./euphrates/comp.scm"
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
%use (list-or-map) "./euphrates/list-or-map.scm"
%use (list-find-first) "./euphrates/list-find-first.scm"
%use (compose-under) "./euphrates/compose-under.scm"
%use (read-string-line) "./euphrates/read-string-line.scm"
%use (list-last) "./euphrates/list-last.scm"

%use (categorization-split) "./categorization-split.scm"

%use (debugv) "./euphrates/debugv.scm"

(define (tegfs-edit-tags working-file)
  (unless working-file
    (raisu 'must-provide-working-file))

  (system-fmt "$EDITOR ~a" working-file)
  (tegfs-process-categorization-text (read-string-file working-file)))

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
  (comp symbol->string string->list
        ((compose-under
          and
          (comp (list-or-map (lambda (c) (member c '(#\* #\=)))))
          (comp list-last (equal? #\=) not)))))

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

(define (list-singleton? lst)
  (and (not (null? lst))
       (null? (cdr lst))))

;; transitive over symbols that have single parent
(define (get-parents/transitive/reflexive ast/flatten ast/flatten/unstarred closed tag/starred)
  (define (get initial)
    (let loop ((tag/starred initial) (ast ast/flatten) (buf '()))
      (when (member tag/starred buf)
        (raisu 'cycle-detected! tag/starred buf))

      (let* ((immediate-parents
              (filter
               identity
               (map
                (lambda (production)
                  (and (member tag/starred (cdr production))
                       (car production)))
                ast)))
             (parent/starred
              (if (list-singleton? immediate-parents)
                  (car immediate-parents)
                  (list-find-first (fn member % closed) #f immediate-parents))))
        (if parent/starred
            (let ((parent (unstar-symbol parent/starred)))
              (cons parent (loop parent ast/flatten/unstarred (cons tag/starred buf))))
            '()))))

  (define ret
    ((curry-if null? (const (get (unstar-symbol tag/starred))))
     (get tag/starred)))
  (cons tag/starred ret))

;; returns either `(ok ,list-of-chosen-tags)
;;             or `(error ,list-of-ambiguous-tags-with-parents)
;;             or `(duplicates)
(define (tegfs-process-categorization-text text)
  (define-values (cfg-part rules-part)
    (categorization-split text))

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

  (if (not (= (length (list-deduplicate starred/unstarred))
              (length starred/unstarred)))
      (cons 'duplicates 'chose-some-tags-twice)
      (let ()
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

        (define main-production
          (car (car ast/flatten)))

        (define (closure closed tag/starred)
          (get-parents/transitive/reflexive ast/flatten ast/flatten/unstarred closed tag/starred))

        (define transitive-closures
          (map (comp (closure '())) starred))

        (define closed
          (list-deduplicate
           (map unstar-symbol
                (apply append
                       (filter
                        (comp (member main-production))
                        transitive-closures)))))

        (define (ambiguous? tag/starred)
          (define cl (closure closed tag/starred))
          (not (member main-production cl)))

        (define ambiguous
          (filter ambiguous? starred))

        (if (null? ambiguous)
            (cons 'ok
                  (filter
                   (negate type-symbol?)
                   (filter
                    (negate (comp (equal? main-production)))
                    (list-deduplicate
                     (map unstar-symbol (apply append transitive-closures))))))
            (cons
             'error
             (map
              (lambda (amb)
                (define unstarred (unstar-symbol amb))
                (cons unstarred
                      (map unstar-symbol (get-parents ast/flatten/unstarred unstarred))))
              ambiguous))))))
