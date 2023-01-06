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

%use (define-tuple) "./euphrates/define-tuple.scm"
%use (fn) "./euphrates/fn.scm"
%use (hashmap-ref) "./euphrates/hashmap.scm"
%use (list-singleton?) "./euphrates/list-singleton-q.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (string-split/simple) "./euphrates/string-split-simple.scm"
%use (web::bad-request) "./web-bad-request.scm"
%use (web::body-not-found) "./web-body-not-found.scm"
%use (web::callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-body callcontext-query callcontext-token) "./web-callcontext.scm"
%use (web::return) "./web-return.scm"
%use (webcore::ask) "./webcore-ask.scm"

(define (web::authcont)
  (define callctx (web::callcontext/p))
  (define key (callcontext-token callctx))
  (define query (callcontext-query callctx))
  (define yes-continue (hashmap-ref query 'yes #f))
  (define no-continue (hashmap-ref query 'no #f))
  (define expected-key (hashmap-ref query 'expected #f))
  (define body/bytes (callcontext-body callctx))

  (cond
   ((not body/bytes)
    (web::body-not-found))
   ((not yes-continue)
    (web::bad-request "Missing query argument ~s" "yes"))
   ((not no-continue)
    (web::bad-request "Missing query argument ~s" "no"))
   ((not expected-key) ;; TODO: allow skipping this?
    (web::bad-request "Missing query argument ~s" "expected"))
   (else
    (let ()
      (define body
        (bytevector->string body/bytes "utf-8"))

      (define parts
        (string-split/simple body #\&))

      (define key-values
        (map (fn string-split/simple % #\=) parts))

      (define _2
        (unless (list-singleton? key-values)
          (raisu 'too-many-query-parameters key-values)))

      (define-tuple (p-key password)
        (car key-values))

      (define _3
        (unless (equal? "psw" p-key)
          (raisu 'bad-query-key p-key)))

      (define result
        (webcore::ask
         `(whats
           (login ,password)
           (key K))))

      (define cont
        (case (car result)
          ((its)
           (let ()
             (define word (cadr result))
             (define token (list-ref word 2))
             (if (equal? token expected-key)
                 yes-continue
                 no-continue)))
          (else no-continue)))

      (web::return
       301
       `((Location . ,cont)
         (Cache-Control . "no-cache"))
       #f)))))
