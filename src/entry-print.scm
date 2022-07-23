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

%var entry-print

%use (entry-registry-path-key) "./entry-registry-path-key.scm"

(define (prop-print prop)
  (define key (car prop))
  (define val (cdr prop))
  (if (equal? entry-registry-path-key key) #f
      (begin
        (display "(")
        (cond
         ((symbol? key) (display (symbol->string key)))
         (else (write key)))
        (cond
         ((string? val)
          (display " . ")
          (write val))
         ((symbol? val)
          (display " . ")
          (display (symbol->string val)))
         ((list? val)
          (for-each (lambda (elem) (display " ") (display (symbol->string elem))) val)))
        (display ")"))))

(define (entry-print entry)
  (display "(")
  (let loop ((buf entry) (first? #t))
    (unless (null? buf)
      (let ((prop (car buf)))
        (unless first? (display "\n "))
        (loop (cdr buf) (not (prop-print prop))))))
  (display ")"))
