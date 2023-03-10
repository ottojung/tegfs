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
  (define-module (tegfs web-callcontext)
    :export (callcontext-ctr callcontext? callcontext-url callcontext-path callcontext-headers callcontext-query callcontext-body callcontext-respheaders set-callcontext-respheaders! add-callcontext-respheaders! callcontext-token)
    :use-module ((euphrates define-type9) :select (define-type9))
    )))



(define-type9 <callcontext>
  (callcontext-ctr url path headersfn queryfn body respheaders tokenfn) callcontext?
  (url callcontext-url) ;; request url
  (path callcontext-path) ;; path part of the url
  (headersfn callcontext-headersfn) ;; request headers
  (queryfn callcontext-queryfn) ;; query hashmap
  (body callcontext-body) ;; client body
  (respheaders callcontext-respheaders set-callcontext-respheaders!) ;; additional response headers to be set
  (tokenfn callcontext-tokenfn) ;; current token function
  )

(define (callcontext-token callctx)
  ((callcontext-tokenfn callctx)))

(define (callcontext-query callctx)
  ((callcontext-queryfn callctx)))

(define (callcontext-headers callctx)
  ((callcontext-headersfn callctx)))

(define (add-callcontext-respheaders! callctx additional)
  (define current (or (callcontext-respheaders callctx) '()))
  (set-callcontext-respheaders!
   callctx (append current additional)))
