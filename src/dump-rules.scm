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

%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (comp) "./euphrates/comp.scm"
%use (define-tuple) "./euphrates/define-tuple.scm"
%use (file-or-directory-exists?) "./euphrates/file-or-directory-exists-q.scm"
%use (list-split-on) "./euphrates/list-split-on.scm"
%use (open-file-port) "./euphrates/open-file-port.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (read-string-line) "./euphrates/read-string-line.scm"
%use (string-split/simple) "./euphrates/string-split-simple.scm"
%use (string->words) "./euphrates/string-to-words.scm"
%use (write-string-file) "./euphrates/write-string-file.scm"
%use (fatal) "./fatal.scm"
%use (get-root) "./get-root.scm"
%use (parse-tag) "./parse-tag.scm"
%use (print-prolog-inference) "./print-prolog-inference.scm"
%use (make-prolog-cut-symbol) "./prolog-cut-symbol.scm"
%use (rules-filename) "./rules-filename.scm"
%use (tags-this-variable/string) "./tags-this-variable.scm"

(define (parse-inference args)
  (define split (list-split-on (comp (equal? "=>")) args))
  (define len (length split))

  (unless (equal? 2 len)
    (fatal "Inference ~s should have had precisely one => arrow, but it has ~s of them" args (- len 1)))

  (define-tuple (antecedents consequents) split)
  (for-each (lambda (c) (print-prolog-inference antecedents c))
            consequents))

(define (parse-synonyms args)
  (define len (length args))
  (unless (< 1 len)
    (fatal "Found an empty synonym"))

  (define main (car args))
  (define rest (cdr args))
  (for-each
   (lambda (synonym)
     (print-prolog-inference (list main) (list synonym))
     (print-prolog-inference (list synonym) (list main)))
   rest))

(define (parse-symmetric args)
  (define len (length args))
  (unless (equal? 1 len)
    (fatal "Symmetric rule ~s should have had exactly one argument, but it has ~s" args len))

  (define name (car args))
  (define parsed ((parse-tag tags-this-variable/string) name))
  (define parsed-length (length parsed))
  (unless (equal? 1 parsed-length)
    (fatal "Parsed symmetric relation ~s should be simple (without args), but it has ~s arguments"
           args (- parsed-length 1)))
  (print-prolog-inference (list (string-append name "=X,Y") (make-prolog-cut-symbol))
                          (string-append name "=Y,X")))

(define (parse-rule words)
  (define command (string->symbol (car words)))
  (define args (cdr words))
  (case command
    ((rule) (parse-inference args))
    ((synonyms) (parse-synonyms args))
    ((symmetric) (parse-symmetric args))
    (else (raisu 'unknown-command words command))))

(define (dump-rules)
  (define rules-file (append-posix-path (get-root) rules-filename))
  (define rules-port
    (begin
      (unless (file-or-directory-exists? rules-file)
        (write-string-file
         rules-file
         ";; This file is for logical rules for tags\n"))
      (open-file-port rules-file "r")))
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
