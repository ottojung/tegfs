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

%var webcore::time-left

%use (profun-set) "./euphrates/profun-accept.scm"
%use (make-profun-error) "./euphrates/profun-error.scm"
%use (profun-op-lambda) "./euphrates/profun-op-lambda.scm"
%use (profun-request-value) "./euphrates/profun-request-value.scm"
%use (profun-bound-value? profun-unbound-value?) "./euphrates/profun-value.scm"
%use (permission-time-left) "./permission-time-left.scm"
%use (tegfs-login-by-key) "./tegfs-login-by-key.scm"

(define (webcore::time-left web::context)
  (profun-op-lambda
   (ctx (K TL) (key-name time-left-name))

   (define perm (tegfs-login-by-key web::context K))

   (cond
    ((profun-bound-value? TL)
     (make-profun-error
      'type-error
      "TL is the return value, it should not be bound"
      time-left-name))

    ((profun-unbound-value? K)
     (profun-request-value key-name))

    ((not perm)
     (make-profun-error
      'permission-denied "Such key does not exist" K))

    (else
     (profun-set
      (time-left-name <- (permission-time-left perm)))))))
