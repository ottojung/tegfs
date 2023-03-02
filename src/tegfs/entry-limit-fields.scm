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
  (define-module (tegfs entry-limit-fields)
    :export (entry-limit-fields)
    :use-module ((tegfs keyword-mimetype) :select (keyword-mimetype))
    :use-module ((tegfs keyword-target) :select (keyword-target))
    :use-module ((tegfs keyword-title) :select (keyword-title))
    :use-module ((tegfs webcore-access) :select (has-access-for-entry-details? has-access-for-entry-target?))
    )))



(define target-fields
  (list keyword-target
        keyword-title
        keyword-mimetype))

(define (entry-limit-fields filemap/2 perm entry0)
  (cond
   ((has-access-for-entry-details? filemap/2 perm entry0)
    entry0)
   ((has-access-for-entry-target? filemap/2 perm entry0)
    (filter (lambda (p) (memq (car p) target-fields)) entry0))
   (else '())))
