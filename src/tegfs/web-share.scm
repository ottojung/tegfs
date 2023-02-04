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
  (define-module (tegfs web-share)
    :export (web::share)
    :use-module ((euphrates assq-or) :select (assq-or))
    :use-module ((euphrates catchu-case) :select (catchu-case))
    :use-module ((euphrates hashmap) :select (hashmap-ref))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((euphrates string-to-seconds) :select (string->seconds))
    :use-module ((euphrates string-to-words) :select (string->words))
    :use-module ((euphrates stringf) :select (stringf))
    :use-module ((tegfs default-share-expiery-time) :select (default-share-expiery-time))
    :use-module ((tegfs web-bad-request) :select (web::bad-request))
    :use-module ((tegfs web-body-to-hashmap) :select (web::body->hashmap))
    :use-module ((tegfs web-callcontext-p) :select (web::callcontext/p))
    :use-module ((tegfs web-callcontext) :select (callcontext-body callcontext-query callcontext-token))
    :use-module ((tegfs web-create-temp-path) :select (web::create-temp-path))
    :use-module ((tegfs web-decode-query) :select (web::decode-query))
    :use-module ((tegfs web-get-query) :select (web::get-query))
    :use-module ((tegfs web-handle-profun-results) :select (web::handle-profun-results))
    :use-module ((tegfs web-make-html-response) :select (web::make-html-response))
    :use-module ((tegfs web-not-found) :select (web::not-found))
    :use-module ((tegfs web-share-default-page) :select (web::share::get-default-text))
    :use-module ((tegfs web-share-settings-page) :select (web::share::get-settings-text))
    :use-module ((tegfs webcore-ask) :select (webcore::ask))
    )))



(define (get-share-duration callctx)
  (define ctxq (callcontext-query callctx))
  (define for-duration/s
    (or
     (hashmap-ref ctxq 'for-duration #f)
     (let* ((body/bytes (callcontext-body callctx))
            (bH (web::body->hashmap body/bytes)))
       (and bH
            (hashmap-ref bH 'for-duration #f)))))

  (if for-duration/s
      (catchu-case
       (string->seconds for-duration/s)
       (('bad-format-for-string->seconds . args)
        (web::bad-request "Bad `for-duration' value ~s" for-duration/s)))
      default-share-expiery-time))

(define (web::share-cont/2 callctx query? first-binding)
  (define token
    (assq-or 'K first-binding (raisu 'unexpected-result-from-backend first-binding)))
  (define location
    (if query?
        (stringf "query?q=&show-filter=off&key=~a" token)
        (assq-or 'FL first-binding (raisu 'unexpected-result-from-backend first-binding))))
  (define share-time
    (assq-or 'AD first-binding (raisu 'unexpected-result-from-backend first-binding)))
  (define password
    (assq-or 'P first-binding (raisu 'unexpected-result-from-backend first-binding)))

  (define yes-continue
    (web::create-temp-path callctx share-time (const location)))

  (define no-continue
    (web::create-temp-path
     callctx
     share-time
     (lambda (no-continue)
       (stringf "auth?yes=~a&no=~a&expected=~a&failed=on"
                yes-continue
                no-continue
                token))))

  (define protected-link
    (web::create-temp-path
     callctx
     share-time
     (lambda _
       (stringf "auth?yes=~a&no=~a&expected=~a"
                yes-continue
                no-continue
                token))))

  (define unprotected-link
    (web::create-temp-path callctx share-time (const location)))

  (define body
    (web::share::get-default-text
     callctx
     share-time
     unprotected-link
     protected-link
     password
     ))

  (web::make-html-response body))

(define (web::share-cont callctx query?)
  (lambda (equals)
    (if (null? equals)
        (web::not-found)
        (web::share-cont/2 callctx query? (car equals)))))

(define (web::share-query query/encoded)
  (define callctx (web::callcontext/p))
  (define key (callcontext-token callctx))
  (define share-duration (get-share-duration callctx))
  (define query (web::decode-query query/encoded))
  (define query/split (string->words query))

  (define result
    (webcore::ask
     `(whats
       (key ,key)
       (make-temporary-permissions ,share-duration AD P K)
       (query ,query/split)
       (entry _E)
       (share-entry _E K)
       more (99999)
       )))

  (web::handle-profun-results
   result (web::share-cont callctx #t)))

(define (web::share-vid senderid)
  (define callctx (web::callcontext/p))
  (define key (callcontext-token callctx))
  (define share-duration (get-share-duration callctx))

  (define result
    (webcore::ask
     `(whats
       (key ,key)
       (make-temporary-permissions ,share-duration AD P K)
       (senderid->entry ,senderid _E)
       (share-entry _E K)
       (share-full _E ,share-duration _2 _F)
       (link-shared _F FL)
       more (99999)
       )))

  (web::handle-profun-results
   result (web::share-cont callctx #f)))

(define (web::share-id id)
  (define callctx (web::callcontext/p))
  (define key (callcontext-token callctx))
  (define share-duration (get-share-duration callctx))

  (define result
    (webcore::ask
     `(whats
       (key ,key)
       (make-temporary-permissions ,share-duration AD P K)
       (entry _E)
       (entry-field _E "id" ,id)
       (share-entry _E K)
       (share-full _E ,share-duration _2 _F)
       (link-shared _F FL)
       more (99999)
       )))

  (web::handle-profun-results
   result (web::share-cont callctx #f)))

(define (web::share::settings)
  (define callctx (web::callcontext/p))
  (define body (web::share::get-settings-text callctx))
  (web::make-html-response body))

(define (web::share)
  (define ctxq (web::get-query))
  (define query/encoded (hashmap-ref ctxq 'q #f))
  (define vid (hashmap-ref ctxq 'vid #f))
  (define id (hashmap-ref ctxq 'id #f))
  (define settings? (hashmap-ref ctxq 'settings #f))

  (cond
   ((equal? settings? "yes") (web::share::settings))
   (query/encoded (web::share-query query/encoded))
   (vid (web::share-vid vid))
   (id (web::share-id id))
   (else (web::bad-request "Bad arguments to share"))))
