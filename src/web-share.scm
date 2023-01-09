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
%use (hashmap-copy hashmap-ref hashmap-set!) "./euphrates/hashmap.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (string->seconds) "./euphrates/string-to-seconds.scm"
%use (string->words) "./euphrates/string-to-words.scm"
%use (stringf) "./euphrates/stringf.scm"
%use (default-share-expiery-time) "./default-share-expiery-time.scm"
%use (web::bad-request) "./web-bad-request.scm"
%use (web::callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-query callcontext-token) "./web-callcontext.scm"
%use (web::create-temp-path) "./web-create-temp-path.scm"
%use (web::decode-query) "./web-decode-query.scm"
%use (web::form-template) "./web-form-template.scm"
%use (web::get-domainname) "./web-get-domainname.scm"
%use (web::get-query) "./web-get-query.scm"
%use (web::handle-profun-results) "./web-handle-profun-results.scm"
%use (web::hashmap->query) "./web-hashmap-to-query.scm"
%use (web::make-html-response) "./web-make-html-response.scm"
%use (webcore::ask) "./webcore-ask.scm"

(define web::share::inside-template
  "
          <div class='tiled-v-element split-container with-separator'>
            <div class='form-block split-left'>
              <label for='username'>Default link</label>
              <input readonly autofocus onfocus='this.select()' value='~a' type='text'/>
            </div>
            <div class='split-right'>
              <div class='form-block'>
                <label for='username'>Protected link</label>
                <input readonly value='~a' type='text'/>
              </div>
              <div class='form-block'>
                <label for='password'>Password</label>
                <input readonly value='~a' type='text'/>
              </div>
            </div>
          </div>
")

(define web::share::outside-template
  "
      <br/>
      <div class='form-block tiled-v-element'>
        <a href='~a'>
          <img src='/static/gear.svg' width='40px' />
        </a>
      </div>
")

(define (get-share-query-text callctx unprotected-link0 protected-link0 password)
  (define domainname (web::get-domainname callctx))
  (define (get-link url0)
    (string-append domainname url0))

  (define unprotected-link
    (get-link unprotected-link0))
  (define protected-link
    (get-link protected-link0))

  (define insides
    (stringf web::share::inside-template
             unprotected-link protected-link password))

  (define settings-query
    (let ((original (hashmap-copy (callcontext-query callctx))))
      (hashmap-set! original 'settings "true")
      original))
  (define settings-link
    (string-append
     "/share?"
     (web::hashmap->query settings-query)))
  (define outsides
    (stringf web::share::outside-template
             settings-link))

  (web::form-template #f insides outsides))

(define (get-share-duration)
  (define ctxq (web::get-query))
  (define for-duration/s
    (hashmap-ref ctxq 'for-duration #f))

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
    (get-share-query-text
     callctx
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
  (define share-duration (get-share-duration))
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
  (define share-duration (get-share-duration))

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

(define (web::share)
  (define ctxq (web::get-query))
  (define query/encoded (hashmap-ref ctxq 'q #f))
  (define vid (hashmap-ref ctxq 'vid #f))

  (cond
   (query/encoded (web::share-query query/encoded))
   (vid (web::share-vid vid))
   (else (web::bad-request "Bad arguments to share"))))
