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

%run guile

%var web::logout

%use (assq-or) "./euphrates/assq-or.scm"
%use (web::callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-headers) "./web-callcontext.scm"
%use (web::get-cookie) "./web-get-cookie.scm"
%use (web::return) "./web-return.scm"
%use (web::set-cookie-header) "./web-set-cookie-header.scm"

(define (web::logout)
  (define callctx (web::callcontext/p))
  (define headers (callcontext-headers callctx))
  (define temp-login? (web::get-cookie "key" headers))
  (define user-login? (web::get-cookie "pwdtoken" headers))
  (define referer0
    (assq-or 'referer headers #f))
  (define referer
    (or referer0 "home"))

  (web::return
   301
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
