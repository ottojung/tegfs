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

%var make-temporary-permissions-handler

%use (profun-set) "./euphrates/profun-accept.scm"
%use (make-profun-error) "./euphrates/profun-error.scm"
%use (profun-op-lambda) "./euphrates/profun-op-lambda.scm"
%use (profun-request-value) "./euphrates/profun-request-value.scm"
%use (profun-bound-value?) "./euphrates/profun-value.scm"
%use (make-permission!) "./make-permission-bang.scm"
%use (permission-token) "./permission.scm"

(define make-temporary-permissions-handler
  (lambda (web-context)
    (profun-op-lambda
     (ctx (D P) (D-name K-name))

     (cond
      ((profun-bound-value? P)
       (make-profun-error 'result-variable-should-be-unbound P))
      ((profun-bound-value? D)
       (let ()
         (define live-duration D)
         (define admin? #f)
         (define detailsaccess? #f) ;; TODO: maybe allow sometimes
         (define share-longer-than-view? #f) ;; TODO: maybe allow sometimes
         (define perm
           (make-permission! web-context live-duration admin? detailsaccess? share-longer-than-view?))
         (define its-key (permission-token perm))
         (profun-set
          (K-name <- its-key))))
      (else
       (profun-request-value D-name))))))
