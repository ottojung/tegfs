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
  (define-module (tegfs web-full)
    :export (web::full)
    :use-module ((euphrates hashmap) :select (hashmap-ref))
    :use-module ((tegfs web-callcontext-p) :select (web::callcontext/p))
    :use-module ((tegfs web-callcontext) :select (callcontext-query))
    :use-module ((tegfs web-get-target-link) :select (web::get-target-link))
    :use-module ((tegfs web-iterate-profun-results) :select (web::iterate-profun-results))
    :use-module ((tegfs web-not-found) :select (web::not-found))
    :use-module ((tegfs web-redirect) :select (web::redirect))
    :use-module ((tegfs webcore-ask) :select (webcore::ask))
    )))



(define (web::full)
  (define callctx (web::callcontext/p))
  (define ctxq (callcontext-query callctx))
  (define senderid (hashmap-ref ctxq 'vid #f))

  (define result
    (webcore::ask
     `(whats
       (link-shared ,senderid L))))

  (web::iterate-profun-results
   result (L)
   (if L
       (let ((target (web::get-target-link L)))
         (web::redirect callctx target #f))
       (web::not-found))))
