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
  (define-module (tegfs print-prolog-inference)
    :export (print-prolog-inference)
    :use-module ((euphrates list-intersperse) :select (list-intersperse))
    :use-module ((tegfs tag-to-prolog-term) :select (print-tag-as-prolog-term tag->prolog-term)))))



(define (print-prolog-inference thing)
  (define consequent-part (cadr thing))
  (define RHS-parts (cddr thing))
  (define RHS
    (apply string-append (list-intersperse ", " (map tag->prolog-term RHS-parts))))
  (print-tag-as-prolog-term consequent-part)
  (unless (null? RHS-parts)
    (display " :- ")
    (display RHS))
  (display ".\n"))
