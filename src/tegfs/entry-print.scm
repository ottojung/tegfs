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
  (define-module (tegfs entry-print)
    :export (entry-print)
    )))

(define (primitive-print elem port)
  (cond
   ((symbol? elem) (write elem port))
   (else (write elem port))))

(define (prop-print prop port)
  (define key (car prop))
  (define val (cdr prop))
  (display "(" port)
  (primitive-print key port)
  (cond
   ((string? val)
    (display " . " port)
    (write val port))
   ((symbol? val)
    (display " . " port)
    (write val port))
   ((list? val)
    (for-each
     (lambda (elem)
       (display " " port)
       (primitive-print elem port))
     val)))
  (display ")" port))

(define (entry-print/full entry port)
  (display "(" port)
  (let loop ((buf entry) (first? #t))
    (unless (null? buf)
      (let ((prop (car buf)))
        (unless first? (display "\n " port))
        (loop (cdr buf) (not (prop-print prop port))))))
  (display ")" port))

(define entry-print
  (case-lambda
   ((entry) (entry-print/full entry (current-output-port)))
   ((entry port) (entry-print/full entry port))))
