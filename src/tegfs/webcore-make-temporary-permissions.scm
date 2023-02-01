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
  (define-module (tegfs webcore-make-temporary-permissions)
    :export (webcore::make-temporary-permissions)
    :use-module ((euphrates profun-accept) :select (profun-set))
    :use-module ((euphrates profun-error) :select (make-profun-error))
    :use-module ((euphrates profun-op-lambda) :select (profun-op-lambda))
    :use-module ((euphrates profun-request-value) :select (profun-request-value))
    :use-module ((euphrates profun-value) :select (profun-bound-value? profun-unbound-value?))
    :use-module ((tegfs get-random-network-name) :select (get-random-network-name))
    :use-module ((tegfs make-permission-bang) :select (make-permission!))
    :use-module ((tegfs permission-time-left) :select (permission-time-left))
    :use-module ((tegfs permission) :select (permission-stime permission-token))
    :use-module ((tegfs webcore-access) :select (can-share-longer-than-view?))
    :use-module ((tegfs webcore-parameters) :select (webcore::permissions/p))
    )))



(define (generate-random-password)
  (get-random-network-name))

(define webcore::make-temporary-permissions
  (lambda (web::context)
    (profun-op-lambda
     :with-env
     (ctx (live-duration actual-duration password key) (live-duration-name actual-duration-name password-name key-name))

     (define perm (webcore::permissions/p))
     (define sharing-time-cap
       (and perm
            (if (can-share-longer-than-view? perm)
                +inf.0
                (permission-time-left perm))))

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
         (define dynamic '())  ;; TODO: maybe allow something sometimes
         (define perm
           (make-permission! web::context live-duration*
                             admin? maybepassword dynamic))
         (define its-key (permission-token perm))
         (define actual (permission-stime perm))

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
