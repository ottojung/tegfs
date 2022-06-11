;;;; Copyright (C) 2022  Otto Jung
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; version 3 of the License.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

%run guile

%var entries->hashmap

%use (make-hashmap hashmap-set!) "./euphrates/ihashmap.scm"

%use (entries-for-each) "./entries-for-each.scm"

(define entries->hashmap
  (case-lambda
   (() (let ((H (make-hashmap))) (entries->hashmap H) H))
   ((H)
    (entries-for-each
     (lambda (entry)
       (define id-pair (assoc 'id entry))
       (if id-pair
           (hashmap-set! H (cdr id-pair) entry)
           (parameterize ((current-output-port (current-error-port)))
             (dprintln "Entry does not have an id: ~s" entry))))))))
