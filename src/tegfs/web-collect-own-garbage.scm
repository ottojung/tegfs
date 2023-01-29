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

%var web::collect-own-garbage

%use (hashmap-delete! hashmap-foreach) "./euphrates/hashmap.scm"
%use (current-time/p) "./current-time-p.scm"
%use (web::current-temp-paths-table/p) "./web-current-temp-paths-table-p.scm"
%use (web::temp-path-still-valid?) "./web-temp-path-still-valid-huh.scm"

(define (web::collect-own-garbage)
  (define table (web::current-temp-paths-table/p))
  (define now (current-time/p))
  (hashmap-foreach
   (lambda (tempid tpath)
     (unless (web::temp-path-still-valid? tpath)
       (hashmap-delete! table tempid)))
   table))
