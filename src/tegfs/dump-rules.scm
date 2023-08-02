;;;; Copyright (C) 2022, 2023  Otto Jung
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU Affero General Public License as published
;;;; by the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU Affero General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Affero General Public License
;;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(cond-expand
 (guile
  (define-module (tegfs dump-rules)
    :export (dump-rules)
    :use-module ((euphrates append-posix-path) :select (append-posix-path))
    :use-module ((euphrates comp) :select (comp))
    :use-module ((euphrates define-tuple) :select (define-tuple))
    :use-module ((euphrates file-or-directory-exists-q) :select (file-or-directory-exists?))
    :use-module ((euphrates list-split-on) :select (list-split-on))
    :use-module ((euphrates open-file-port) :select (open-file-port))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((euphrates read-string-line) :select (read-string-line))
    :use-module ((euphrates string-split-simple) :select (string-split/simple))
    :use-module ((euphrates string-to-words) :select (string->words))
    :use-module ((euphrates write-string-file) :select (write-string-file))
    :use-module ((tegfs fatal) :select (fatal))
    :use-module ((tegfs get-root) :select (get-root))
    :use-module ((tegfs parse-term) :select (parse-term))
    :use-module ((tegfs prolog-cut-symbol) :select (make-prolog-cut-symbol))
    :use-module ((tegfs prolog-query-parse) :select (prolog-query-parse))
    :use-module ((tegfs rules-filename) :select (rules-filename))
    :use-module ((tegfs tags-this-variable) :select (tags-this-variable/string))
    )))



(define (parse-inference yield args)
  (define split (list-split-on (comp (equal? "=>")) args))
  (define len (length split))

  (unless (equal? 2 len)
    (fatal "Inference ~s should have had precisely one => arrow, but it has ~s of them" args (- len 1)))

  (define-tuple (antecedents consequents) split)
  (define-values (RHS-parts RHS-variables)
    (prolog-query-parse antecedents))
  (for-each (lambda (c)
              (define consequent (car (prolog-query-parse (list c))))
              (yield (cons 'inference (cons consequent RHS-parts))))
            consequents))

(define (parse-synonyms yield args)
  (define len (length args))
  (unless (< 1 len)
    (fatal "Found an empty synonym"))

  (define main (car args))
  (define rest (cdr args))
  (for-each
   (lambda (synonym)
     (yield (list 'inference (string->symbol synonym) (string->symbol main)))
     (yield (list 'inference (string->symbol main) (string->symbol synonym))))
   rest))

(define (parse-symmetric yield args)
  (define len (length args))
  (unless (equal? 1 len)
    (fatal "Symmetric rule ~s should have had exactly one argument, but it has ~s" args len))

  (define name (car args))
  (define parsed ((parse-term tags-this-variable/string) name))
  (define parsed-length (length parsed))
  (unless (equal? 1 parsed-length)
    (fatal "Parsed symmetric relation ~s should be simple (without args), but it has ~s arguments"
           args (- parsed-length 1)))
  (yield
   (list 'inference
         (list (string->symbol name) 'Y 'X)
         (list (string->symbol name) 'X 'Y) (make-prolog-cut-symbol))))

(define (parse-rule yield)
  (lambda (words)
    (define command (string->symbol (car words)))
    (define args (cdr words))
    (case command
      ((rule) (parse-inference yield args))
      ((synonyms) (parse-synonyms yield args))
      ((symmetric) (parse-symmetric yield args))
      (else (raisu 'unknown-command words command)))))

(define (dump-rules yield)
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

  (for-each (parse-rule yield) rules-commands))
