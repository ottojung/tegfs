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

%var permission-time-left

%use (time-get-current-unixtime) "./euphrates/time-get-current-unixtime.scm"
%use (permission-start permission-time) "./permission.scm"

(define permission-time-left
  (case-lambda
   ((perm)
    (permission-time-left perm (time-get-current-unixtime)))
   ((perm current-time)
    (define end (+ (permission-start perm)
                   (permission-time perm)))
    (max 0 (- end current-time)))))
