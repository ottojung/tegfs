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
  (define-module (tegfs parsed-categorization-tags-get-all)
    :export (parsed-categorization-tags-get-all)
    :use-module ((euphrates list-deduplicate) :select (list-deduplicate))
    )))



(define (parsed-categorization-tags-get-all ast/flatten)
  (define words
    (apply append ast/flatten))

  (define without-special
    (if (null? words) '()
        (cdr words)))

  (list-deduplicate without-special))
