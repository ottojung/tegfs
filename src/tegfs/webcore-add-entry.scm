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
  (define-module (tegfs webcore-add-entry)
    :export (webcore::add-entry)
    :use-module ((euphrates profun-accept) :select (profun-set-meta))
    :use-module ((euphrates profun-error) :select (make-profun-error))
    :use-module ((euphrates profun-op-lambda) :select (profun-op-lambda))
    :use-module ((euphrates profun-request-value) :select (profun-request-value))
    :use-module ((euphrates profun-value) :select (profun-unbound-value?))
    :use-module ((tegfs add-entry) :select (add-entry))
    :use-module ((tegfs permission) :select (permission?))
    :use-module ((tegfs webcore-access) :select (can-upload?))
    :use-module ((tegfs webcore-get-current-permissions) :select (webcore::get-current-permissions))
    )))



(define webcore::add-entry
  (profun-op-lambda
   :with-env
   (ctx (entry) (E-name))

   (define perm (webcore::get-current-permissions))

   (cond
    ((not (permission? perm))
     (make-profun-error 'permission-denied "Not authorized. Missing key?"))
    ((not (can-upload? perm))
     (make-profun-error 'permission-denied "This user cannot create new entries"))
    ((profun-unbound-value? entry)
     (profun-request-value E-name))

    (else
     (let ((created (add-entry entry)))
       (profun-set-meta
        (E-name <- created)))))))
