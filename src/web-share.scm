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

%var web-share

%use (assq-or) "./euphrates/assq-or.scm"
%use (catchu-case) "./euphrates/catchu-case.scm"
%use (hashmap-ref) "./euphrates/hashmap.scm"
%use (profune-communicator-handle) "./euphrates/profune-communicator.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (string->seconds) "./euphrates/string-to-seconds.scm"
%use (string->words) "./euphrates/string-to-words.scm"
%use (stringf) "./euphrates/stringf.scm"
%use (~s) "./euphrates/tilda-s.scm"
%use (words->string) "./euphrates/words-to-string.scm"
%use (default-share-expiery-time) "./default-share-expiery-time.scm"
%use (get-random-access-token) "./get-random-access-token.scm"
%use (keyword-id) "./keyword-id.scm"
%use (web-bad-request) "./web-bad-request.scm"
%use (web-callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-request) "./web-callcontext.scm"
%use (web-context/p) "./web-context-p.scm"
%use (web-decode-query) "./web-decode-query.scm"
%use (web-get-key) "./web-get-key.scm"
%use (web-get-query) "./web-get-query.scm"
%use (web-make-communicator) "./web-make-communicator.scm"
%use (web-request-get-domainname) "./web-request-get-domainname.scm"
%use (web-respond) "./web-respond.scm"
%use (web-static-error-message) "./web-static-error-message.scm"

%for (COMPILER "guile")

(use-modules (sxml simple))

%end

(define (print-url url)
  (sxml->xml `(a (@ (href ,url)) ,url)))

(define (get-share-query-text callctx location hidden-query-location token)
  (define req (callcontext-request callctx))
  (define domainname
    (web-request-get-domainname req))
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
  (define ctxq (web-get-query))
  (define for-duration/s
    (hashmap-ref ctxq 'for-duration #f))

  (if for-duration/s
      (catchu-case
       (string->seconds for-duration/s)
       (('bad-format-for-string->seconds . args)
        ((web-static-error-message
          417
          (stringf "Bad `for-duration' value ~s" for-duration/s)))))
      default-share-expiery-time))

(define (web-share-cont3 ctx callctx query/encoded equals)
  (define req (callcontext-request callctx))
  (define domainname (web-request-get-domainname req))
  (define first-binding (car equals))
  (define token
    (assq-or 'K first-binding (raisu 'unexpected-result-from-backend equals)))
  (define location
    (if query/encoded
        (stringf "~a/query?q=~a&key=~a" domainname query/encoded token)
        (assq-or 'FL first-binding (raisu 'unexpected-result-from-backend equals))))
  (define hidden-query-location
    (stringf "~a/query?q=%any&key=~a" domainname token))
  (define text
    (get-share-query-text callctx location hidden-query-location token))
  (web-respond text))

(define (web-share-cont2 ctx callctx query/encoded result)
  (define equals (cadr (cadr result)))
  (if (null? equals)
      (web-bad-request "Permission denied")
      (web-share-cont3 ctx callctx query/encoded equals)))

(define (web-share-cont ctx callctx query/encoded result)
  (if (equal? (car result) 'its)
      (web-share-cont2 ctx callctx query/encoded result)
      (web-bad-request
       "Could not share the query: ~a"
       (words->string (map ~s result)))))

(define (web-share-query query/encoded)
  (define ctx (web-context/p))
  (define callctx (web-callcontext/p))
  (define key (web-get-key callctx))
  (define share-duration (get-share-duration))
  (define query (web-decode-query query/encoded))
  (define query/split (string->words query))

  (define result
    (profune-communicator-handle
     (web-make-communicator (web-context/p))
     `(whats
       (key ,key)
       (make-temporary-permissions ,share-duration K)
       (query ,query/split)
       (entry _E)
       (share-entry _E K)
       more (99999)
       )))

  (web-share-cont ctx callctx query/encoded result))

(define (web-share-id id)
  (define ctx (web-context/p))
  (define callctx (web-callcontext/p))
  (define key (web-get-key callctx))
  (define share-duration (get-share-duration))
  (define query/split '("%any"))

  (define result
    (profune-communicator-handle
     (web-make-communicator (web-context/p))
     `(whats
       (key ,key)
       (make-temporary-permissions ,share-duration K)
       (query ,query/split)
       (entry _E)
       (entry-field _E "id" ,id)
       (share-entry _E K)
       (share-full _E ,share-duration _F)
       (link-shared _F FL)
       more (99999)
       )))

  (web-share-cont ctx callctx #f result))

(define (web-share)
  (define ctxq (web-get-query))
  (define query/encoded (hashmap-ref ctxq 'q #f))
  (define id (hashmap-ref ctxq keyword-id #f))

  (cond
   (query/encoded (web-share-query query/encoded))
   (id (web-share-id id))
   (else (web-static-error-message 417 "Bad arguments to share"))))
