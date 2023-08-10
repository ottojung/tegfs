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
  (define-module (tegfs categorization-complete-selection)
    :export (categorization-complete-selection)
    :use-module ((tegfs categorization-complete-selection-parsed) :select (categorization-complete-selection/parsed))
    :use-module ((tegfs categorization-parse) :select (categorization-parse))
    )))

;;
;; Returns `((ok ,list-of-chosen-tags)
;;           (ambiguous ,list-of-ambiguous-tags+their-parents)
;;           (duplicates ,list-of-duplicates))
;;
;; With "ok" field always present,
;; but "ambiguous" or "duplicates" may be absent.
;;

(define (categorization-complete-selection categorization-text starred)
  (define ast/flatten
    (categorization-parse categorization-text))
  (define additional-rules '()) ;; TODO: parse it from `text`

  (categorization-complete-selection/parsed
   ast/flatten additional-rules starred))
