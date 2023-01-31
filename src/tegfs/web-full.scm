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
    :use-module ((tegfs web-get-query) :select (web::get-query))
    :use-module ((tegfs web-iterate-profun-results) :select (web::iterate-profun-results))
    :use-module ((tegfs web-not-found) :select (web::not-found))
    :use-module ((tegfs web-return) :select (web::return))
    :use-module ((tegfs webcore-ask) :select (webcore::ask))
    )))



(define (web::full)
  (define ctxq (web::get-query))
  (define senderid (hashmap-ref ctxq 'vid #f))

  (define result
    (webcore::ask
     `(whats
       (link-shared ,senderid L)
       )))

  (web::iterate-profun-results
   result (L)
   (if L
       (web::return
        303
        `((Location . ,L)
          (Cache-Control . "no-cache"))
        #f)
       (web::not-found))))
