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
  (define-module (tegfs entry-print)
    :export (entry-print)
    :use-module ((tegfs keyword-entry-registry-path) :select (keyword-entry-registry-path)))))



(define (primitive-print elem)
  (cond
   ((symbol? elem) (display (symbol->string elem)))
   (else (write elem))))

(define (prop-print prop)
  (define key (car prop))
  (define val (cdr prop))
  (if (equal? keyword-entry-registry-path key) #f
      (begin
        (display "(")
        (primitive-print key)
        (cond
         ((string? val)
          (display " . ")
          (write val))
         ((symbol? val)
          (display " . ")
          (display (symbol->string val)))
         ((list? val)
          (for-each (lambda (elem) (display " ") (primitive-print elem)) val)))
        (display ")"))))

(define (entry-print entry)
  (display "(")
  (let loop ((buf entry) (first? #t))
    (unless (null? buf)
      (let ((prop (car buf)))
        (unless first? (display "\n "))
        (loop (cdr buf) (not (prop-print prop))))))
  (display ")"))
