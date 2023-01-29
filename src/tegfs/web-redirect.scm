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
  (define-module (tegfs web-redirect)
    :export (web::redirect)
    :use-module ((euphrates url-get-path) :select (url-get-path))
    :use-module ((euphrates url-get-protocol) :select (url-get-protocol))
    :use-module ((euphrates url-get-query) :select (url-get-query))
    :use-module ((tegfs web-callcontext) :select (callcontext-headers))
    :use-module ((tegfs web-make-callcontext) :select (web::make-callcontext/raw))
    :use-module ((tegfs web-return) :select (web::return))
    :use-module ((tegfs web-server-current-handler-p) :select (webcore::server-current/p)))))



(define (web::redirect callctx new-url new-body)
  (define handler (webcore::server-current/p))
  (define old-headers (callcontext-headers callctx))
  (define new-path (url-get-path new-url))
  (define new-query/encoded (url-get-query new-url))
  (define new-callctx
    (web::make-callcontext/raw
     new-url new-path new-query/encoded old-headers
     new-body))

  (if (string-null? (url-get-protocol new-url))
      (handler new-callctx)
      (web::return
       303
       `((Location . ,new-url)
         (Cache-Control . "no-cache"))
       #f)))
