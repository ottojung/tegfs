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
  (define-module (tegfs web-query)
    :export (web::query)
    :use-module ((euphrates hashmap) :select (hashmap-ref))
    :use-module ((euphrates string-to-words) :select (string->words))
    :use-module ((tegfs default-full-sharing-time) :select (default-full-sharing-time))
    :use-module ((tegfs default-preview-sharing-time) :select (default-preview-sharing-time))
    :use-module ((tegfs web-body-to-hashmap) :select (web::body->hashmap))
    :use-module ((tegfs web-callcontext-p) :select (web::callcontext/p))
    :use-module ((tegfs web-callcontext) :select (callcontext-body callcontext-query callcontext-token))
    :use-module ((tegfs web-decode-query) :select (web::decode-query))
    :use-module ((tegfs web-handle-profun-results) :select (web::handle-profun-results))
    :use-module ((tegfs web-make-html-response) :select (web::make-html-response))
    :use-module ((tegfs web-query-display-results) :select (web::query-display-results))
    :use-module ((tegfs webcore-ask) :select (webcore::ask))
    )))



(define (web::query)
  (define callctx (web::callcontext/p))
  (define ctxq (callcontext-query callctx))

  (define select? (equal? "on" (hashmap-ref ctxq 'select #f)))
  (define show-filter?/0 (not (equal? "off" (hashmap-ref ctxq 'show-filter #f))))
  (define show-filter? (and show-filter?/0 (not select?)))

  (define query/submitted/0
    (or
     (hashmap-ref ctxq 'q #f)
     (let* ((body (callcontext-body callctx))
            (bH (and body (web::body->hashmap body))))
       (and bH
            (hashmap-ref bH 'q #f)))))
  (define query/submitted
    (if (or (not query/submitted/0)
            (string-null? query/submitted/0)) #f
            query/submitted/0))
  (define query/encoded
    (or query/submitted "%any"))
  (define query (web::decode-query query/encoded))
  (define query/split (string->words query))

  (define result
    (and query/submitted/0
         (webcore::ask
          `(whats
            (key ,(callcontext-token callctx))
            (query ,query/split)
            (entry E)
            (share-preview E ,default-preview-sharing-time _ATP _P)
            (share-full E ,default-full-sharing-time _ATF F)
            (link-shared _P PL)
            more (99999)
            ))))

  (web::handle-profun-results
   (or result '(its (false)))
   (web::query-handle-results callctx show-filter? select? query/submitted query)))

(define (web::query-handle-results callctx show-filter? select? query/submitted query)
  (lambda (equals)
    (web::make-html-response
     (lambda _
       (define maybe-value
         (if (and query/submitted (not select?))
             (string-append " value='" query "'")
             ""))

       (define initial?
         (and (null? equals)
              (not query/submitted)))
       (define show-menu?
         (and (not select?)
              (not initial?)))

       (web::query-display-results callctx initial? select? show-filter? maybe-value show-menu? equals)))))
