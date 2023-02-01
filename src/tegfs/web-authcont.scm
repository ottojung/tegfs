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

(cond-expand
 (guile
  (define-module (tegfs web-authcont)
    :export (web::authcont)
    :use-module ((euphrates hashmap) :select (hashmap-ref))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((tegfs web-bad-request) :select (web::bad-request))
    :use-module ((tegfs web-body-not-found) :select (web::body-not-found))
    :use-module ((tegfs web-body-to-hashmap) :select (web::body->hashmap))
    :use-module ((tegfs web-callcontext) :select (callcontext-body))
    :use-module ((tegfs web-iterate-profun-results) :select (web::iterate-profun-results))
    :use-module ((tegfs web-return) :select (web::return))
    :use-module ((tegfs web-set-cookie-header) :select (web::set-cookie-header))
    :use-module ((tegfs webcore-ask) :select (webcore::ask))
    )))



(define (web::authcont callctx query)
  (define yes-continue (hashmap-ref query 'yes #f))
  (define expected-key (hashmap-ref query 'expected ""))
  (define no-continue (hashmap-ref query 'no #f))
  (define temporary-v (hashmap-ref query 'temporary #f))
  (define temporary? (equal? temporary-v "off"))
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

      (define name "")
      (define password
        (or (hashmap-ref key-values 'psw #f)
            (raisu 'bad-body-key)))

      (define result
        (webcore::ask
         `(whats
           (login ,name ,password)
           (key K)
           (time-left K TL)
           )))

      (define (fail-fun results)
        (values no-continue 0 #f))

      (define-values (cont expiery token)
        (web::iterate-profun-results
         :or fail-fun
         :results result
         (K TL)
         (if (or (equal? K expected-key)
                 (and K (string-null? expected-key)))
             (values yes-continue TL K)
             (values no-continue 0 #f))))

      (web::return
       303
       (append
        `((Location . ,cont)
          (Cache-Control . "no-cache"))
        (if token
            (list
             (web::set-cookie-header
              (if temporary? "key" "pwdtoken")
              token
              expiery))
            '()))
       #f)))))
