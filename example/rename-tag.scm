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

;; This is an example that renames tag "image" to "picture"
;;  by making modifications in every entry of every registry

%run guile

%use (assoc-or) "./euphrates/assoc-or.scm"
%use (assoc-set-value) "./euphrates/assoc-set-value.scm"
%use (curry-if) "./euphrates/curry-if.scm"
%use (comp) "./euphrates/comp.scm"

%use (entries-map!) "./tegfs/entries-map-bang.scm"

(define from 'image)
(define to 'picture)

(entries-map!
 (lambda (entry)
   (define tags (assoc-or 'tags entry '()))
   (define new-tags
     (map (curry-if (comp (equal? from)) (const to)) tags))
   (assoc-set-value 'tags new-tags entry)))
