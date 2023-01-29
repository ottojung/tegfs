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

(cond-expand
 (guile
  (define-module (tegfs entries-to-hashmap)
    :export (entries->hashmap)
    :use-module ((euphrates dprintln) :select (dprintln))
    :use-module ((euphrates hashmap) :select (hashmap-set! make-hashmap))
    :use-module ((tegfs entries-for-each) :select (entries-for-each))
    :use-module ((tegfs keyword-id) :select (keyword-id)))))



(define entries->hashmap
  (case-lambda
   (() (let ((H (make-hashmap))) (entries->hashmap H) H))
   ((H)
    (entries-for-each
     (lambda (entry)
       (define id-pair (assoc keyword-id entry))
       (if id-pair
           (hashmap-set! H (cdr id-pair) entry)
           (parameterize ((current-output-port (current-error-port)))
             (dprintln "Entry does not have an id: ~s" entry))))))))
