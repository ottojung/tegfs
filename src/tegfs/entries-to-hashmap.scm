;;;; Copyright (C) 2022, 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define entries->hashmap
  (case-lambda
   (() (let ((H (make-hashmap))) (entries->hashmap H) H))
   ((H)
    (entries-for-each
     (lambda (entry)
       (define id-pair (assoc keyword-id entry))
       (if id-pair
           (hashmap-set! H (cdr id-pair) entry)
           (log-warning "Entry does not have an id: ~s" entry)))))))
