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

(cond-expand
 (guile
  (define-module (example rename-tag)
    :use-module ((euphrates assoc-or) :select (assoc-or))
    :use-module ((euphrates assoc-set-value) :select (assoc-set-value))
    :use-module ((euphrates curry-if) :select (curry-if))
    :use-module ((euphrates comp) :select (comp))
    :use-module ((tegfs entries-map-bang) :select (entries-map!)))))

(define from 'image)
(define to 'picture)

(entries-map!
 (lambda (registry-path entry)
   (define tags (assoc-or 'tags entry '()))
   (define new-tags
     (map (curry-if (comp (equal? from)) (const to)) tags))
   (assoc-set-value 'tags new-tags entry)))
