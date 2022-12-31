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

%var web-share-query

%use (assq-or) "./euphrates/assq-or.scm"
%use (catchu-case) "./euphrates/catchu-case.scm"
%use (hashmap-ref) "./euphrates/hashmap.scm"
%use (profune-communicator-handle) "./euphrates/profune-communicator.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (string->seconds) "./euphrates/string-to-seconds.scm"
%use (stringf) "./euphrates/stringf.scm"
%use (~s) "./euphrates/tilda-s.scm"
%use (words->string) "./euphrates/words-to-string.scm"
%use (default-share-expiery-time) "./default-share-expiery-time.scm"
%use (get-random-access-token) "./get-random-access-token.scm"
%use (web-bad-request) "./web-bad-request.scm"
%use (web-callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-request) "./web-callcontext.scm"
%use (web-context/p) "./web-context-p.scm"
%use (web-get-key) "./web-get-key.scm"
%use (web-get-query) "./web-get-query.scm"
%use (web-make-communicator) "./web-make-communicator.scm"
%use (web-request-get-domainname) "./web-request-get-domainname.scm"
%use (web-respond) "./web-respond.scm"

(define (get-share-query-text callctx location hidden-query-location token)
  (define req (callcontext-request callctx))
  (define domainname
    (web-request-get-domainname req))
  (define (print-link url0)
    (define url (string-append domainname url0))
    (print-url url))
  (define (print-newline)
    (display "<br>\n"))
  (define (print-line title url)
    (display title)
    (display ":")
    (print-newline)
    (print-link url)
    (print-newline))

  (with-output-to-string
    (lambda _
      (parameterize ((current-error-port (current-output-port)))
        (print-line "Default link" location)
        (print-line "Hidden query link" hidden-query-location)
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
        ((static-error-message
          417
          (stringf "Bad `for-duration' value ~s" for-duration/s)))))
      default-share-expiery-time))

(define (web-share-query query/encoded)
  (define callctx (web-callcontext/p))
  (define key (web-get-key callctx))
  (define share-duration (get-share-duration))

  (define result
    (profune-communicator-handle
     (web-make-communicator (web-context/p))
     `(whats
       (key ,key)
       (query ,query/split)
       (entry _E)
       (make-temporary-permissions ,share-duration K)
       (share-entry _E K)
       more (99999)
       )))

  (if (equal? (car result) 'its)
      (let ()
        (define equals (cadr (cadr result)))
        (define first-binding (car equals))
        (define token
          (assq-or 'K first-binding (raisu 'unexpected-result-from-backend equals)))
        (define location
          (stringf "/query?q=~a&key=~a" query/encoded token))
        (define hidden-query-location
          (stringf "/query?q=%any&key=~a" token))
        (define text
          (get-share-query-text callctx location hidden-query-location token))
        (web-respond text))
      (web-bad-request
       "Could not share the query: ~a"
       (words->string (map ~s result)))))
