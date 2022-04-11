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

%var dump-rules


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
%use (list-ref-or) "./euphrates/list-ref-or.scm"
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
%use (list-find-first) "./euphrates/list-find-first.scm"
%use (compose-under) "./euphrates/compose-under.scm"
%use (read-string-line) "./euphrates/read-string-line.scm"
%use (open-file-port) "./euphrates/open-file-port.scm"
%use (list-span-while) "./euphrates/list-span-while.scm"
%use (alphanum/alphabet/index) "./euphrates/alphanum-alphabet.scm"
%use (~a) "./euphrates/tilda-a.scm"
%use (list-deduplicate/reverse) "./euphrates/list-deduplicate.scm"
%use (string->words) "./euphrates/string-to-words.scm"
%use (list-intersperse) "./euphrates/list-intersperse.scm"

%use (categorization-filename) "./categorization-filename.scm"
%use (rules-filename) "./rules-filename.scm"
%use (tags-this-variable/string) "./tags-this-variable.scm"
%use (root/p) "./root-p.scm"
%use (get-registry-files) "./get-registry-files.scm"
%use (parse-tag) "./parse-tag.scm"
%use (tag->prolog-term print-tag-as-prolog-term) "./tag-to-prolog-term.scm"
%use (entries-for-each) "./entries-for-each.scm"
%use (id-name) "./id-name.scm"
%use (query-parse) "./query-parse.scm"

;; TODO: better errors

(define (print-inference antecedents consequents)
  (for-each
   (lambda (consequent)
     (define-values (RHS-parts RHS-variables)
       (query-parse antecedents))
     (define RHS
       (apply string-append (list-intersperse ", " (map tag->prolog-term RHS-parts))))
     (define-values (consequent-parts consequent-variables)
       (query-parse (list consequent)))
     (print-tag-as-prolog-term (car consequent-parts))
     (display " :- ")
     (display RHS)
     (display ".\n"))
   consequents))

(define (parse-inference args)
  (define split (list-split-on (comp (equal? "=>")) args))
  (define len (length split))

  (unless (equal? 2 len)
    (raisu 'bad-inference-length len args))

  (define-tuple (antecedents consequents) split)
  (print-inference antecedents consequents))

(define (parse-synonyms args)
  (define len (length args))
  (unless (< 1 len)
    (raisu 'bad-synonym-length len args))

  (define main (car args))
  (define rest (cdr args))
  (for-each
   (lambda (synonym)
     (print-inference (list main) (list synonym))
     (print-inference (list synonym) (list main)))
   rest))

(define (parse-symmetric args)
  (define len (length args))
  (unless (equal? 1 len)
    (raisu 'bad-symmetric-length len args))

  (define name (car args))
  (define parsed ((parse-tag tags-this-variable/string) (string->symbol name)))
  (define parsed-length (length parsed))
  (unless (equal? 1 parsed-length)
    (raisu 'bad-symmetric-parsed-length parsed-length args))
  (print-inference (list (string-append name "=X,Y"))
                   (list (string-append name "=Y,X"))))

(define (parse-rule words)
  (define command (string->symbol (car words)))
  (define args (cdr words))
  (case command
    ((rule) (parse-inference args))
    ((synonyms) (parse-synonyms args))
    ((symmetric) (parse-symmetric args))
    (else (raisu 'unknown-command words command))))

(define (dump-rules)
  (define rules-file (append-posix-path (root/p) rules-filename))
  (define rules-port (open-file-port rules-file "r"))
  (define rules-commands
    (let loop ((buf '()))
      (define line (read-string-line rules-port))
      (if (eof-object? line)
          (reverse buf)
          (let* ((uncommented (car (string-split/simple line #\;)))
                 (words (string->words uncommented)))
            (if (null? words)
                (loop buf)
                (loop (cons words buf)))))))

  (for-each parse-rule rules-commands))
