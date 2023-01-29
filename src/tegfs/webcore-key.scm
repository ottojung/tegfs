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
  (define-module (tegfs webcore-key)
    :export (webcore::key)
    :use-module ((euphrates profun-accept) :select (profun-set profun-set-parameter))
    :use-module ((euphrates profun-op-lambda) :select (profun-op-lambda))
    :use-module ((euphrates profun-request-value) :select (profun-request-value))
    :use-module ((euphrates profun-value) :select (profun-bound-value?))
    :use-module ((tegfs permission) :select (permission-token))
    :use-module ((tegfs tegfs-login-by-key) :select (tegfs-login-by-key))
    :use-module ((tegfs webcore-parameters) :select (webcore::permissions/p)))))



(define webcore::key
  (lambda (tegfs-context)
    (profun-op-lambda
     :with-env
     (ctx (key) (K-name))

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
