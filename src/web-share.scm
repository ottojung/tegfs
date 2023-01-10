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

%run guile

%var web::share

%use (assq-or) "./euphrates/assq-or.scm"
%use (catchu-case) "./euphrates/catchu-case.scm"
%use (hashmap-ref) "./euphrates/hashmap.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (string->seconds) "./euphrates/string-to-seconds.scm"
%use (string->words) "./euphrates/string-to-words.scm"
%use (stringf) "./euphrates/stringf.scm"
%use (default-share-expiery-time) "./default-share-expiery-time.scm"
%use (web::bad-request) "./web-bad-request.scm"
%use (web::body->hashmap) "./web-body-to-hashmap.scm"
%use (web::callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-body callcontext-query callcontext-token) "./web-callcontext.scm"
%use (web::create-temp-path) "./web-create-temp-path.scm"
%use (web::decode-query) "./web-decode-query.scm"
%use (web::get-query) "./web-get-query.scm"
%use (web::handle-profun-results) "./web-handle-profun-results.scm"
%use (web::make-html-response) "./web-make-html-response.scm"
%use (web::share::get-default-text) "./web-share-default-page.scm"
%use (web::share::get-settings-text) "./web-share-settings-page.scm"
%use (webcore::ask) "./webcore-ask.scm"

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
        (stringf "/query?q=~a&key=~a" "%any" token)
        (assq-or 'FL first-binding (raisu 'unexpected-result-from-backend first-binding))))
  (define share-time
    (assq-or 'AD first-binding (raisu 'unexpected-result-from-backend first-binding)))
  (define password
    (assq-or 'P first-binding (raisu 'unexpected-result-from-backend first-binding)))

  (define yes-continue
    (web::create-temp-path share-time location))
  (define no-continue
    (web::create-temp-path
     share-time
     (lambda _
       (stringf "/authfail?yes=~a&no=~a&expected=~a"
                yes-continue
                no-continue
                token))))

  (define protected-link
    (web::create-temp-path
     share-time
     (stringf "/auth?yes=~a&no=~a&expected=~a"
              yes-continue
              no-continue
              token)))

  (define unprotected-link
    (web::create-temp-path share-time location))

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
        (raisu 'unexpected-false-from-backend-71631231237)
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

(define (web::share::settings)
  (define callctx (web::callcontext/p))
  (define body (web::share::get-settings-text callctx))
  (web::make-html-response body))

(define (web::share)
  (define ctxq (web::get-query))
  (define query/encoded (hashmap-ref ctxq 'q #f))
  (define vid (hashmap-ref ctxq 'vid #f))
  (define settings? (hashmap-ref ctxq 'settings #f))

  (cond
   ((equal? settings? "yes") (web::share::settings))
   (query/encoded (web::share-query query/encoded))
   (vid (web::share-vid vid))
   (else (web::bad-request "Bad arguments to share"))))
