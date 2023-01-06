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
%use (permission-time permission-token) "./permission.scm"
%use (webcore::permissions/p) "./webcore-parameters.scm"

(define (generate-random-password)
  (get-random-network-name))

(define make-temporary-permissions-handler
  (lambda (web::context)
    (profun-op-lambda
     :with-env
     (ctx (live-duration actual-duration password key) (live-duration-name actual-duration-name password-name key-name))

     (define perm (webcore::permissions/p))
     (define sharing-time-cap
       (and perm (permission-time-left perm)))

     (cond
      ((profun-bound-value? key)
       (make-profun-error 'type-error "Key is the result and should not be set" key-name))
      ((profun-unbound-value? live-duration)
       (profun-request-value live-duration-name))
      ((and (profun-bound-value? password)
            (not (or (string? password) (not password))))
       (make-profun-error 'type-error "Password must be either a string or #f"))
      ((not (and (number? live-duration)
                 (< 0 live-duration)))
       ;; TODO: accept strings as in string->seconds
       (make-profun-error 'type-error "Duration must be a positive number" live-duration-name live-duration))
      ((not perm)
       (make-profun-error 'permission-denied "Not authorized to create temporary users"))
      (else
       (let ()
         (define live-duration* (min sharing-time-cap live-duration))
         (define admin? #f)
         (define maybepassword
           (if (profun-bound-value? password) password
               (generate-random-password)))
         (define uploadaccess? #f) ;; TODO: maybe allow sometimes
         (define detailsaccess? #f) ;; TODO: maybe allow sometimes
         (define share-longer-than-view? #f) ;; TODO: maybe allow sometimes
         (define perm
           (make-permission! web::context live-duration* admin? maybepassword uploadaccess? detailsaccess? share-longer-than-view?))
         (define its-key (permission-token perm))
         (define actual (permission-time perm))

         (define r0
           (profun-set
            (key-name <- its-key)))

         (define r1
           (if (profun-bound-value? password) r0
               (profun-set (password-name <- maybepassword) r0)))

         (define r2
           (if (profun-bound-value? actual-duration)
               (if (equal? actual actual-duration) r1
                   (make-profun-error 'permission-denied "Cannot create user that lives for that long"))
               (profun-set
                (actual-duration-name <- actual) r1)))

         r2))))))
