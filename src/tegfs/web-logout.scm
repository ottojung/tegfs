;;;; Copyright (C) 2023  Otto Jung
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
  (define-module (tegfs web-logout)
    :export (web::logout)
    :use-module ((tegfs web-callcontext-p) :select (web::callcontext/p))
    :use-module ((tegfs web-callcontext) :select (callcontext-headers))
    :use-module ((tegfs web-get-cookie) :select (web::get-cookie))
    :use-module ((tegfs web-get-safe-referer) :select (web::get-safe-referer))
    :use-module ((tegfs web-return) :select (web::return))
    :use-module ((tegfs web-set-cookie-header) :select (web::set-cookie-header))
    )))



(define (web::logout)
  (define callctx (web::callcontext/p))
  (define headers (callcontext-headers callctx))
  (define temp-login? (web::get-cookie "key" headers))
  (define user-login? (web::get-cookie "pwdtoken" headers))
  (define referer
    (web::get-safe-referer callctx))

  (web::return
   303
   (append
    `((Location . ,referer)
      (Cache-Control . "no-cache"))
    (cond
     (temp-login?
      (list (web::set-cookie-header "key" "" 0)))
     (user-login?
      (list (web::set-cookie-header "pwdtoken" "" 0)))
     (else '())))
   #f))
