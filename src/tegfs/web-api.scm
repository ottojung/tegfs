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
  (define-module (tegfs web-api)
    :export (web::api)
    :use-module ((euphrates conss) :select (conss))
    :use-module ((euphrates hashmap) :select (hashmap-ref))
    :use-module ((euphrates tilda-s) :select (~s))
    :use-module ((tegfs web-bad-request) :select (web::bad-request))
    :use-module ((tegfs web-callcontext-p) :select (web::callcontext/p))
    :use-module ((tegfs web-callcontext) :select (callcontext-token))
    :use-module ((tegfs web-decode-query) :select (web::decode-query))
    :use-module ((tegfs web-decode-sentence) :select (web::decode-sentence))
    :use-module ((tegfs web-get-query) :select (web::get-query))
    :use-module ((tegfs web-handle-profun-results) :select (web::handle-profun-results))
    :use-module ((tegfs web-return) :select (web::return))
    :use-module ((tegfs webcore-ask) :select (webcore::ask))
    )))


(define (web::api)
  (define ctxq (web::get-query))
  (define callctx (web::callcontext/p))
  (define key (callcontext-token callctx))
  (define sentence/0 (hashmap-ref ctxq 'stc #f))
  (define sentence/1
    (and sentence/0 (web::decode-query sentence/0)))
  (define sentence/2
    (and sentence/1 (web::decode-sentence sentence/1)))

  (cond
   ((not key)
    (web::bad-request "Missing key parameter"))
   ((not sentence/2)
    (web::bad-request "Missing stc parameter"))
   (else
    (let ()
      (define sentence
        (conss `whats
               `(key ,key)
               `(check-api-access)
               sentence/2))
      (define result
        (webcore::ask sentence))
      (web::handle-profun-results
       result
       (lambda (equals)
         (web::return
          200
          `((content-type . (text/plain)))
          (~s equals))))))))
