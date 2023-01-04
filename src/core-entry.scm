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

%run guile

%var core::entry

%use (profun-set) "./euphrates/profun-accept.scm"
%use (profun-reject) "./euphrates/profun-reject.scm"
%use (core-entry-handler/generic) "./core-entry-handler-generic.scm"
%use (tegfs-query/open) "./tegfs-query-open.scm"

(define (core::entry-get-iter opening-properties query E-name)
  (define iter0 (tegfs-query/open opening-properties query))
  (define (iter)
    (define x (iter0))
    (if x
        (profun-set (E-name <- x))
        (profun-reject)))
  iter)

(define core::entry
  (core-entry-handler/generic core::entry-get-iter))
