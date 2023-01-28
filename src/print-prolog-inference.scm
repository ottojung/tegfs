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

%run guile

%var print-prolog-inference

%use (list-intersperse) "./euphrates/list-intersperse.scm"
%use (prolog-query-parse) "./prolog-query-parse.scm"
%use (print-tag-as-prolog-term tag->prolog-term) "./tag-to-prolog-term.scm"

(define (print-prolog-inference antecedents consequent)
  (define-values (RHS-parts RHS-variables)
    (prolog-query-parse antecedents))
  (define RHS
    (apply string-append (list-intersperse ", " (map tag->prolog-term RHS-parts))))
  (define-values (consequent-parts consequent-variables)
    (prolog-query-parse (list consequent)))
  (print-tag-as-prolog-term (car consequent-parts))
  (unless (null? RHS-parts)
    (display " :- ")
    (display RHS))
  (display ".\n"))
