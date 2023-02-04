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
  (define-module (tegfs webcore-check-api-access)
    :export (webcore::check-api-access)
    :use-module ((euphrates profun-accept) :select (profun-accept))
    :use-module ((euphrates profun-error) :select (make-profun-error))
    :use-module ((euphrates profun-op-lambda) :select (profun-op-lambda))
    :use-module ((tegfs webcore-access) :select (has-api-access?))
    :use-module ((tegfs webcore-get-current-permissions) :select (webcore::get-current-permissions))
    )))



(define webcore::check-api-access
  (lambda (web::context)
    (profun-op-lambda
     :with-env
     (ctx env ())

     (define perm (webcore::get-current-permissions))

     (if (has-api-access? perm)
         (profun-accept)
         (make-profun-error
          'permission-denied
          "This user cannot access the API directly")))))
