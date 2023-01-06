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

%var web::authfail

%use (hashmap-ref) "./euphrates/hashmap.scm"
%use (web::callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-query callcontext-token) "./web-callcontext.scm"
%use (web::get-auth-body) "./web-get-auth-body.scm"
%use (web::make-html-response) "./web-make-html-response.scm"

(define (web::authfail)
  (define callctx (web::callcontext/p))
  (define failed? #t)
  (define key (callcontext-token callctx))
  (define query (callcontext-query callctx))
  (define yes-continue (hashmap-ref query 'yes #f))
  (define no-continue (hashmap-ref query 'no #f))
  (define expected-key (hashmap-ref query 'expected #f))

  (web::make-html-response
   (web::get-auth-body failed? yes-continue no-continue expected-key)))
