;;;; Copyright (C) 2022  Otto Jung
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
%use (debugs) "./euphrates/debugs.scm"
%use (hashmap-ref) "./euphrates/hashmap.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (string->seconds) "./euphrates/string-to-seconds.scm"
%use (string->words) "./euphrates/string-to-words.scm"
%use (stringf) "./euphrates/stringf.scm"
%use (default-share-expiery-time) "./default-share-expiery-time.scm"
%use (get-random-access-token) "./get-random-access-token.scm"
%use (web::bad-request) "./web-bad-request.scm"
%use (web::callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-token) "./web-callcontext.scm"
%use (web::create-temp-path) "./web-create-temp-path.scm"
%use (web::decode-query) "./web-decode-query.scm"
%use (web::get-domainname) "./web-get-domainname.scm"
%use (web::get-query) "./web-get-query.scm"
%use (web::handle-profun-results) "./web-handle-profun-results.scm"
%use (web::make-html-response) "./web-make-html-response.scm"
%use (web::not-found) "./web-not-found.scm"
%use (webcore::ask) "./webcore-ask.scm"

%for (COMPILER "guile")

(use-modules (sxml simple))

%end

(define (print-url url)
  (sxml->xml `(a (@ (href ,url)) ,url)))

(define (get-share-query-text callctx location hidden-query-location token)
  (define domainname (web::get-domainname callctx))
  (define (print-link url0)
    (define url (string-append domainname url0))
    (print-url url))
  (define (print-newline)
    (display "<br>\n"))
  (define (print-line/fullink title url)
    (display title)
    (display ":")
    (print-newline)
    (print-url url)
    (print-newline))
  (define (print-line title url)
    (display title)
    (display ":")
    (print-newline)
    (print-link url)
    (print-newline))

  (with-output-to-string
    (lambda _
      (parameterize ((current-error-port (current-output-port)))
        (print-line/fullink "Default link" location)
        (print-line/fullink "Hidden query link" hidden-query-location)
        (print-newline) (print-newline)
        (display "Second then forth:")
        (print-newline)
        (print-link
         (stringf "/query?q=ll&key=~a" (get-random-access-token)))
        (print-newline)
        (print-link
         (stringf "/query?q=ll&key=~a" token))
        (print-newline)
        (print-link
         (stringf "/query?q=ll&key=~a" (get-random-access-token)))
        (print-newline)
        (print-link "/query?q=%any")
        (print-newline)
        (print-link
         (stringf "/query?q=ll&key=~a" (get-random-access-token)))))))

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

;; (define (web::share-cont/2/old callctx query/encoded first-binding)
;;   (define domainname (web::get-domainname callctx))
;;   (define token
;;     (assq-or 'K first-binding (raisu 'unexpected-result-from-backend first-binding)))
;;   (define location
;;     (if query/encoded
;;         (stringf "~a/query?q=~a&key=~a" domainname query/encoded token)
;;         (assq-or 'FL first-binding (raisu 'unexpected-result-from-backend first-binding))))
;;   (define hidden-query-location
;;     (stringf "~a/query?q=%any&key=~a" domainname token))
;;   (define text
;;     (get-share-query-text callctx location hidden-query-location token))
;;   (web::make-html-response text))

(define (web::share-cont/2 callctx query/encoded first-binding)
  (define token
    (assq-or 'K first-binding (raisu 'unexpected-result-from-backend first-binding)))
  (define location
    (if query/encoded
        (stringf "/query?q=~a&key=~a" query/encoded token)
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
     (lambda (tempid)
       (stringf "/authfail?yes=~a&no=~a&expected=~a"
                yes-continue
                tempid
                token))))

  (define initial
    (web::create-temp-path
     share-time
     (stringf "/auth?yes=~a&no=~a&expected=~a"
              yes-continue
              no-continue
              token)))

  (define text
    (stringf "<br><br>~a<br><br>~a"
             initial password))

  (debugs yes-continue)
  (debugs no-continue)
  (debugs initial)

  (web::make-html-response text))

  ;; (define hidden-query-location
  ;;   (stringf "~a/query?q=%any&key=~a" domainname token))
  ;; (define text
  ;;   (get-share-query-text callctx location hidden-query-location token))
  ;; (web::make-html-response text))

(define (web::share-cont callctx query/encoded)
  (lambda (equals)
    (if (null? equals)
        (web::not-found)
        (web::share-cont/2 callctx query/encoded (car equals)))))

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
   result (web::share-cont callctx query/encoded)))

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
