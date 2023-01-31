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
  (define-module (tegfs with-current-time)
    :export (with-current-time)
    :use-module ((euphrates time-get-current-unixtime) :select (time-get-current-unixtime))
    :use-module ((tegfs current-time-p) :select (current-time/p))
    )))



(define-syntax with-current-time
  (syntax-rules ()
    ((_ . bodies)
     (parameterize ((current-time/p (time-get-current-unixtime)))
       (let () . bodies)))))
