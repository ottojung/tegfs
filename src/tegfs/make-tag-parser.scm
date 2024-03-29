;;;; Copyright (C) 2022, 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define (make-tag-parser counter)
  (define term-parser (make-term-parser counter))
  (lambda (tag)
    (define terms (term-parser tag))
    (define (concretisize x)
      (if (number? x) x (~a x)))

    (if (or (symbol? tag) (string? tag))
        (map
         (lambda (parsed-term)
           (cons (car parsed-term)
                 (map concretisize (cdr parsed-term))))
         terms)
        terms)))
