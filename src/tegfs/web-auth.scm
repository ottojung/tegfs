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
  (define-module (tegfs web-auth)
    :export (web::auth)
    :use-module ((euphrates hashmap) :select (hashmap-ref))
    :use-module ((tegfs web-authcont) :select (web::authcont))
    :use-module ((tegfs web-callcontext-p) :select (web::callcontext/p))
    :use-module ((tegfs web-callcontext) :select (callcontext-query))
    :use-module ((tegfs web-get-auth-body) :select (web::get-auth-body))
    :use-module ((tegfs web-make-html-response) :select (web::make-html-response))
    )))



(define (web::authinitial callctx query)
  (define failed-v (hashmap-ref query 'failed 'false))
  (define failed? (equal? failed-v "on"))
  (define temporary-v (hashmap-ref query 'temporary "on"))
  (define yes-continue (hashmap-ref query 'yes #f))
  (define no-continue (hashmap-ref query 'no #f))
  (define expected-key (hashmap-ref query 'expected #f))

  (web::make-html-response
   (web::get-auth-body failed? yes-continue no-continue expected-key temporary-v)))

(define (web::auth)
  (define callctx (web::callcontext/p))
  (define query (callcontext-query callctx))
  (define continue-v (hashmap-ref query 'continue #f))
  (define continue? (equal? "on" continue-v))

  (if continue?
      (web::authcont callctx query)
      (web::authinitial callctx query)))
