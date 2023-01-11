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

%run guile

;; Returns all tags, incuding the starred ones
%var categorization-get-all-tags

%use (list-deduplicate) "./euphrates/list-deduplicate.scm"
%use (categorization-parse-tags) "./categorization-parse-tags.scm"
%use (parsed-categorization-tags-get-all) "./parsed-categorization-tags-get-all.scm"

(define (categorization-get-all-tags categorization-text)
  (define parsed
    (categorization-parse-tags categorization-text))

  (list-deduplicate
   (parsed-categorization-tags-get-all parsed)))
