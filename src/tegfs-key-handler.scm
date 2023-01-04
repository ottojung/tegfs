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

%var tegfs-key-handler

%use (profun-set profun-set-parameter) "./euphrates/profun-accept.scm"
%use (profun-op-envlambda) "./euphrates/profun-op-envlambda.scm"
%use (profun-request-value) "./euphrates/profun-request-value.scm"
%use (profun-bound-value?) "./euphrates/profun-value.scm"
%use (permission-token) "./permission.scm"
%use (webcore::permissions/p) "./webcore-parameters.scm"
%use (tegfs-login-by-key) "./tegfs-login-by-key.scm"

(define tegfs-key-handler
  (lambda (tegfs-context)
    (profun-op-envlambda
     (ctx env (K-name))

     (define key (env K-name))

     (cond
      ((profun-bound-value? key)
       (let ((perm (tegfs-login-by-key tegfs-context key)))
         (profun-set-parameter (webcore::permissions/p <- perm))))
      (else
       (let ((perm (webcore::permissions/p)))
         (if perm
             (let ((key (permission-token perm)))
               (profun-set (K-name <- key)))
             (profun-request-value K-name))))))))
