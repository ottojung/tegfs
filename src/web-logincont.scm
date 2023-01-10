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

%var web::logincont

%use (hashmap-ref) "./euphrates/hashmap.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (web::body-not-found) "./web-body-not-found.scm"
%use (web::body->hashmap) "./web-body-to-hashmap.scm"
%use (web::callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-body) "./web-callcontext.scm"
%use (web::login-failed-body) "./web-login-failed-body.scm"
%use (web::login-success-body) "./web-login-success-body.scm"
%use (web::make-html-response) "./web-make-html-response.scm"
%use (web::set-cookie-header) "./web-set-cookie-header.scm"
%use (webcore::ask) "./webcore-ask.scm"

(define (web::logincont)
  (define body/bytes (callcontext-body (web::callcontext/p)))
  (if (not body/bytes)
      (web::body-not-found)
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
             (key K)
             )))

        (case (car result)
          ((its)
           (let* ((word (cadr result))
                  (token (list-ref word 2)))
             (web::make-html-response
              web::login-success-body
              #:extra-headers (list (web::set-cookie-header "pwdtoken" token)))))
          (else
           (web::make-html-response web::login-failed-body))))))
