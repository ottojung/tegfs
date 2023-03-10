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
  (define-module (tegfs categorization-first-order-tag-huh)
    :export (categorization::first-order-tag?)
    :use-module ((euphrates raisu) :select (raisu))
    )))



(define (categorization::first-order-tag?/string tag)
  (not (or (string-prefix? "<" tag) ;; Group tag like <TYPE>
           (string-suffix? "=" tag) ;; Simple higher order tag
           )))

(define (categorization::first-order-tag? tag)
  (cond
   ((string? tag)
    (categorization::first-order-tag?/string tag))
   ((symbol? tag)
    (categorization::first-order-tag?/string (symbol->string tag)))
   (else
    (raisu 'type-error "Expected a tag, which is either a string or a symbol"))))
