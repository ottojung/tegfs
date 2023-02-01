;;;; Copyright (C) 2022, 2023  Otto Jung
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
  (define-module (tegfs webcore-user)
    :export (webcore::user-make webcore::user? webcore::user-name webcore::user-password)
    :use-module ((euphrates define-type9) :select (define-type9))
    )))

(define-type9 <webcore::user>
  (webcore::user-constructor name password) webcore::user?
  (name webcore::user-name)
  (password webcore::user-password)
  )

(define (webcore::user-make name password)
  (webcore::user-constructor name password))
