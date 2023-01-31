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

(cond-expand
 (guile
  (define-module (tegfs webcore-time-left)
    :export (webcore::time-left)
    :use-module ((euphrates profun-accept) :select (profun-set))
    :use-module ((euphrates profun-error) :select (make-profun-error))
    :use-module ((euphrates profun-op-lambda) :select (profun-op-lambda))
    :use-module ((euphrates profun-request-value) :select (profun-request-value))
    :use-module ((euphrates profun-value) :select (profun-bound-value? profun-unbound-value?))
    :use-module ((tegfs permission-time-left) :select (permission-time-left))
    :use-module ((tegfs tegfs-login-by-key) :select (tegfs-login-by-key))
    )))



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
