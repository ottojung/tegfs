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
%use (profun-op-envlambda) "./euphrates/profun-op-envlambda.scm"
%use (profun-request-value) "./euphrates/profun-request-value.scm"
%use (profun-bound-value? profun-unbound-value?) "./euphrates/profun-value.scm"
%use (make-permission!) "./make-permission-bang.scm"
%use (permission-time-left) "./permission-time-left.scm"
%use (permission-token) "./permission.scm"
%use (webcore::permissions/p) "./webcore-parameters.scm"

(define make-temporary-permissions-handler
  (lambda (web-context)
    (profun-op-envlambda
     (ctx env (D-name K-name))

     (define D (env D-name))
     (define K (env K-name))
     (define perm (webcore::permissions/p))
     (define sharing-time-cap
       (and perm (permission-time-left perm)))

     (cond
      ((profun-bound-value? K)
       (make-profun-error 'result-variable-should-be-unbound K))
      ((profun-unbound-value? D)
       (profun-request-value D-name))
      ((not (and (number? D)
                 (< 0 D)))
       ;; TODO: accept strings as in string->seconds
       (make-profun-error 'type-error "Duration must be a positive number" D))
      ((not perm)
       (make-profun-error 'permission-denied "Not authorized to create temporary users"))
      (else
       (let ()
         (define live-duration-0 D)
         (define live-duration (min sharing-time-cap live-duration-0))
         (define admin? #f)
         (define maybepassword #f) ;; TODO: generate one
         (define uploadaccess? #f) ;; TODO: maybe allow sometimes
         (define detailsaccess? #f) ;; TODO: maybe allow sometimes
         (define share-longer-than-view? #f) ;; TODO: maybe allow sometimes
         (define perm
           (make-permission! web-context live-duration admin? maybepassword uploadaccess? detailsaccess? share-longer-than-view?))
         (define its-key (permission-token perm))
         (profun-set
          (K-name <- its-key))))))))
