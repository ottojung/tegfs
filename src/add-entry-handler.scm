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

%run guile

%var add-entry-handler

%use (profun-accept) "./euphrates/profun-accept.scm"
%use (make-profun-error) "./euphrates/profun-error.scm"
%use (profun-op-envlambda) "./euphrates/profun-op-envlambda.scm"
%use (profun-request-value) "./euphrates/profun-request-value.scm"
%use (profun-unbound-value?) "./euphrates/profun-value.scm"
%use (add-entry) "./add-entry.scm"
%use (permission?) "./permission.scm"
%use (tegfs-permissions/p) "./talk-parameters.scm"

(define add-entry-handler
  (lambda (tegfs-context)
    (profun-op-envlambda
     (ctx env (R-name E-name))

     (define registry-file (env R-name))
     (define entry (env E-name))
     (define perm (tegfs-permissions/p))

     (cond
      ((not (permission? perm))
       (make-profun-error 'permission-denied "Not authorized. Missing key?"))
      ((profun-unbound-value? registry-file)
       (profun-request-value R-name))
      ((profun-unbound-value? entry)
       (profun-request-value E-name))

      (else
       (add-entry registry-file entry)
       (profun-accept))))))
