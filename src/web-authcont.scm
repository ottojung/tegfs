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

%run guile

%var web::authcont

%use (hashmap-ref) "./euphrates/hashmap.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (web::bad-request) "./web-bad-request.scm"
%use (web::body-not-found) "./web-body-not-found.scm"
%use (web::body->hashmap) "./web-body-to-hashmap.scm"
%use (web::callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-body callcontext-query callcontext-token) "./web-callcontext.scm"
%use (web::return) "./web-return.scm"
%use (web::set-cookie-header) "./web-set-cookie-header.scm"
%use (webcore::ask) "./webcore-ask.scm"

(define (web::authcont)
  (define callctx (web::callcontext/p))
  (define key (callcontext-token callctx))
  (define query (callcontext-query callctx))
  (define yes-continue (hashmap-ref query 'yes #f))
  (define expected-key (hashmap-ref query 'expected ""))
  (define no-continue (hashmap-ref query 'no #f))
  (define temporary-v (hashmap-ref query 'temporary #f))
  (define temporary? (equal? temporary-v "no"))
  (define body/bytes (callcontext-body callctx))

  (cond
   ((not body/bytes)
    (web::body-not-found))
   ((not yes-continue)
    (web::bad-request "Missing query argument ~s" "yes"))
   ((not no-continue)
    (web::bad-request "Missing query argument ~s" "no"))
   (else
    (let ()
      (define key-values
        (web::body->hashmap body/bytes))

      (define password
        (or (hashmap-ref key-values 'psw #f)
            (raisu 'bad-body-key)))

      (define result
        (webcore::ask
         `(whats
           (login ,password)
           (key K))))

      (define-values (cont token)
        (case (car result)
          ((its)
           (let ()
             (define word (cadr result))
             (define token (list-ref word 2))
             (if (or (equal? token expected-key)
                     (and token (string-null? expected-key)))
                 (values yes-continue token)
                 (values no-continue #f))))
          (else (values no-continue #f))))

      (web::return
       301
       (append
        `((Location . ,cont)
          (Cache-Control . "no-cache"))
        (if token
            (list
             (web::set-cookie-header
              (if temporary? "key" "pwdtoken")
              token))
            '()))
       #f)))))
