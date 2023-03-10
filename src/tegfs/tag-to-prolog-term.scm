;;;; Copyright (C) 2022  Otto Jung
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
  (define-module (tegfs tag-to-prolog-term)
    :export (tag->prolog-term print-tag-as-prolog-term)
    :use-module ((euphrates alphanum-alphabet) :select (alphanum/alphabet/index))
    :use-module ((euphrates comp) :select (comp))
    :use-module ((euphrates compose-under) :select (compose-under))
    :use-module ((euphrates list-and-map) :select (list-and-map))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((tegfs prolog-cut-symbol) :select (prolog-cut-symbol?))
    :use-module ((tegfs prolog-var) :select (prolog-var-name prolog-var?))
    )))



(define (prolog-var-needs-quoting? var/chars)
  (define first (alphanum/alphabet/index (car var/chars)))
  (not
   (or (equal? '(#\~) var/chars)
       (and first
            (> first 9)
            (< first 36)
            (list-and-map
             (compose-under
              or
              (comp (equal? #\_))
              alphanum/alphabet/index)
             (cdr var/chars))))))

(define (print-prolog-symbol arg)
  (cond
   ((symbol? arg)
    (let* ((str (symbol->string arg))
           (chars (string->list str)))
      (if (prolog-var-needs-quoting? chars)
          (begin
            (display "'")
            (display str)
            (display "'"))
          (display str))))
   ((string? arg) (write arg))
   ((integer? arg) (write arg))
   ((prolog-var? arg) (display (prolog-var-name arg)))
   ((prolog-cut-symbol? arg) (display "!"))
   (else (raisu 'uknown-type arg))))

(define (comma-translate lst)
  (let loop ((lst lst))
    (unless (null? lst)
      (print-prolog-subterm (car lst))
      (unless (null? (cdr lst))
        (display ", "))
      (loop (cdr lst)))))

(define (print-prolog-subterm thing)
  (cond
   ((pair? thing)
    (print-prolog-symbol (car thing))
    (display "(")
    (comma-translate (cdr thing))
    (display ")"))
   (else
    (print-prolog-symbol thing))))

(define (print-tag-as-prolog-term thing)
  (cond
   ((pair? thing)
    (cond
     ((or (null? (cdr thing))
          (null? (cddr thing))
          (null? (cdddr thing)))
      (print-prolog-symbol (car thing))
      (display "(")
      (comma-translate (cdr thing))
      (display ")"))
     (else
      (print-prolog-symbol (car thing))
      (display "(")
      (print-prolog-symbol (cadr thing))
      (display ", [")
      (comma-translate (cddr thing))
      (display "])"))))
   (else
    (print-prolog-symbol thing))))

(define (tag->prolog-term thing)
  (with-output-to-string
    (lambda _ (print-tag-as-prolog-term thing))))
