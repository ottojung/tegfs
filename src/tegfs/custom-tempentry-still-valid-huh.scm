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
  (define-module (tegfs custom-tempentry-still-valid-huh)
    :export (custom-tempentry-still-valid?)
    :use-module ((tegfs current-time-p) :select (current-time/p))
    :use-module ((tegfs custom-tempentry-time-left) :select (custom-tempentry-time-left))
    )))



(define custom-tempentry-still-valid?
  (case-lambda
   ((perm)
    (custom-tempentry-still-valid? perm (current-time/p)))
   ((perm current-time)
    (< 0 (custom-tempentry-time-left perm current-time)))))
