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
  (define-module (tegfs web-login)
    :export (web::login)
    :use-module ((euphrates stringf) :select (stringf))
    :use-module ((euphrates uri-encode) :select (uri-encode))
    :use-module ((tegfs web-callcontext-p) :select (web::callcontext/p))
    :use-module ((tegfs web-get-safe-referer) :select (web::get-safe-referer))
    :use-module ((tegfs web-redirect) :select (web::redirect)))))



(define (web::login)
  (define callctx (web::callcontext/p))
  (define referer (web::get-safe-referer callctx))
  (define target
    (stringf "auth?yes=~a" (uri-encode referer)))

  (web::redirect callctx target #f))
