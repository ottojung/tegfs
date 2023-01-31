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
  (define-module (tegfs webcore-create-admin-permission-bang)
    :export (webcore::create-admin-permission!)
    :use-module ((tegfs make-permission-bang) :select (make-permission!))
    )))



(define (webcore::create-admin-permission! webcore::context maybepassword expiery-time)
  ;; TODO: read below from the config
  (define admin? #t)
  (define dynamic '())

  (make-permission!
   webcore::context
   expiery-time
   admin? maybepassword dynamic))
