;;;; Copyright (C) 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define (maybe->symbol x)
  (define s (~a x))
  (if (equal? tags-this-variable/string s) x
      (string->symbol s)))

(define (make-term-parser counter)
  (define tag-parser (make-tag-parser counter))
  (lambda (term)
    (define whole (tag-parser term))
    (cons (car whole)
          (map maybe->symbol (cdr whole)))))