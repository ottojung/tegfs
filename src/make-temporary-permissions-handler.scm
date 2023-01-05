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
%use (profun-bound-value? profun-unbound-value?) "./euphrates/profun-value.scm"
%use (get-random-network-name) "./get-random-network-name.scm"
%use (make-permission!) "./make-permission-bang.scm"
%use (permission-time-left) "./permission-time-left.scm"
%use (permission-token) "./permission.scm"
%use (webcore::permissions/p) "./webcore-parameters.scm"

(define (generate-random-password)
  (get-random-network-name))

(define make-temporary-permissions-handler
  (lambda (web-context)
    (profun-op-lambda
     :with-env
     (ctx (D P0 K) (D-name P-name K-name))

     (define perm (webcore::permissions/p))
     (define sharing-time-cap
       (and perm (permission-time-left perm)))

     (cond
      ((profun-bound-value? K)
       (make-profun-error 'result-variable-should-be-unbound K))
      ((profun-unbound-value? D)
       (profun-request-value D-name))
      ((and (profun-bound-value? P0)
            (not (or (string? P0) (not P0))))
       (make-profun-error 'type-error "Password must be either a string or #f"))
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
         (define maybepassword
           (if (profun-bound-value? P0) P0
               (generate-random-password)))
         (define uploadaccess? #f) ;; TODO: maybe allow sometimes
         (define detailsaccess? #f) ;; TODO: maybe allow sometimes
         (define share-longer-than-view? #f) ;; TODO: maybe allow sometimes
         (define perm
           (make-permission! web-context live-duration admin? maybepassword uploadaccess? detailsaccess? share-longer-than-view?))
         (define its-key (permission-token perm))

         (define r0
           (profun-set
            (K-name <- its-key)))

         (if (profun-bound-value? P0) r0
             (profun-set (P-name <- maybepassword) r0))))))))
