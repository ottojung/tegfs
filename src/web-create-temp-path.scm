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

%var web::create-temp-path

%use (hashmap-set!) "./euphrates/hashmap.scm"
%use (memconst) "./euphrates/memconst.scm"
%use (current-time/p) "./current-time-p.scm"
%use (get-random-network-name) "./get-random-network-name.scm"
%use (web::current-temp-paths-table/p) "./web-current-temp-paths-table-p.scm"
%use (web::temp-path-ctr) "./web-temp-path.scm"

(define (web::create-temp-path stime destination/0)
  (define now (current-time/p))
  (define table (web::current-temp-paths-table/p))
  (define tempid
    (string-append "/" (get-random-network-name)))
  (define destination
    (if (procedure? destination/0)
        (memconst (destination/0 tempid))
        destination/0))
  (define tpath (web::temp-path-ctr tempid destination now stime))

  (hashmap-set! table tempid tpath)

  tempid)
