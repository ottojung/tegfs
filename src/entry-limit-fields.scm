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

%var entry-limit-fields

%use (has-access-for-entry-details? has-access-for-entry-target?) "./access.scm"
%use (keyword-entry-parent-directory) "./keyword-entry-parent-directory.scm"
%use (keyword-entry-registry-path) "./keyword-entry-registry-path.scm"
%use (keyword-target) "./keyword-target.scm"
%use (keyword-title) "./keyword-title.scm"

(define target-fields
  (list keyword-target
        keyword-title
        keyword-entry-parent-directory ;; FIXME: remove non-target fields
        keyword-entry-registry-path))

(define (entry-limit-fields filemap/2 perm entry0)
  (cond
   ((has-access-for-entry-details? filemap/2 perm entry0)
    entry0)
   ((has-access-for-entry-target? filemap/2 perm entry0)
    (filter (lambda (p) (memq (car p) target-fields)) entry0))
   (else '())))
