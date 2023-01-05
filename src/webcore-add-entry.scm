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

%run guile

%var webcore::add-entry

%use (profun-accept) "./euphrates/profun-accept.scm"
%use (make-profun-error) "./euphrates/profun-error.scm"
%use (profun-op-lambda) "./euphrates/profun-op-lambda.scm"
%use (profun-request-value) "./euphrates/profun-request-value.scm"
%use (profun-unbound-value?) "./euphrates/profun-value.scm"
%use (can-upload?) "./access.scm"
%use (add-entry) "./add-entry.scm"
%use (permission?) "./permission.scm"
%use (webcore::permissions/p) "./webcore-parameters.scm"

(define webcore::add-entry
  (profun-op-lambda
   :with-env
   (ctx (registry-file entry) (R-name E-name))

   (define perm (webcore::permissions/p))

   (cond
    ((not (permission? perm))
     (make-profun-error 'permission-denied "Not authorized. Missing key?"))
    ((not (can-upload? perm))
     (make-profun-error 'permission-denied "This user cannot create new entries"))
    ((profun-unbound-value? registry-file)
     (profun-request-value R-name))
    ((profun-unbound-value? entry)
     (profun-request-value E-name))

    (else
     (add-entry registry-file entry)
     (profun-accept)))))
