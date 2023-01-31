;;;; Copyright (C) 2023  Otto Jung
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
  (define-module (tegfs core-entry)
    :export (core::entry)
    :use-module ((euphrates profun-accept) :select (profun-set))
    :use-module ((euphrates profun-reject) :select (profun-reject))
    :use-module ((tegfs core-entry-generic) :select (core::entry/generic))
    :use-module ((tegfs tegfs-query-open) :select (tegfs-query/open))
    )))



(define (core::entry-get-iter opening-properties query E-name)
  (define iter0 (tegfs-query/open opening-properties query))
  (define (iter)
    (define x (iter0))
    (if x
        (profun-set (E-name <- x))
        (profun-reject)))
  iter)

(define core::entry
  (core::entry/generic core::entry-get-iter))
