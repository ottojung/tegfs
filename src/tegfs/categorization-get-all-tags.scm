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
  (define-module (tegfs categorization-get-all-tags)
    :export (categorization-get-all-tags)
    :use-module ((euphrates list-deduplicate) :select (list-deduplicate))
    :use-module ((tegfs categorization-parse) :select (categorization-parse))
    :use-module ((tegfs parsed-categorization-tags-get-all) :select (parsed-categorization-tags-get-all))
    )))

;; Returns all tags, incuding the starred ones


(define (categorization-get-all-tags categorization-text)
  (define parsed
    (categorization-parse categorization-text))

  (list-deduplicate
   (parsed-categorization-tags-get-all parsed)))
