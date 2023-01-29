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
  (define-module (tegfs web-upload)
    :export (web::upload)
    :use-module ((euphrates hashmap) :select (hashmap-ref))
    :use-module ((tegfs web-callcontext-p) :select (web::callcontext/p))
    :use-module ((tegfs web-callcontext) :select (callcontext-query callcontext-token))
    :use-module ((tegfs web-iterate-profun-results) :select (web::iterate-profun-results))
    :use-module ((tegfs web-make-html-response) :select (web::make-html-response))
    :use-module ((tegfs web-make-upload-body) :select (web::make-upload-body))
    :use-module ((tegfs web-uploadcont) :select (web::uploadcont))
    :use-module ((tegfs webcore-ask) :select (webcore::ask)))))



(define (web::upload::initial callctx)
  (define key (callcontext-token callctx))

  (define result
    (webcore::ask
     `(whats (key ,key) (categorization C))))

  (web::iterate-profun-results
   result (C)
   (web::make-html-response
    (web::make-upload-body C))))

(define (web::upload)
  (define callctx (web::callcontext/p))
  (define query (callcontext-query callctx))
  (define continue-v (hashmap-ref query 'continue #f))
  (define continue? (equal? "on" continue-v))

  (if continue?
      (web::uploadcont)
      (web::upload::initial callctx)))
