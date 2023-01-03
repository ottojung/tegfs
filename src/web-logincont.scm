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

%var web-logincont

%use (define-tuple) "./euphrates/define-tuple.scm"
%use (fn) "./euphrates/fn.scm"
%use (list-singleton?) "./euphrates/list-singleton-q.scm"
%use (profune-communicator-handle) "./euphrates/profune-communicator.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (string-split/simple) "./euphrates/string-split-simple.scm"
%use (web-body-not-found) "./web-body-not-found.scm"
%use (web-callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-body) "./web-callcontext.scm"
%use (web-context/p) "./web-context-p.scm"
%use (web-login-failed-body) "./web-login-failed-body.scm"
%use (web-login-success-body) "./web-login-success-body.scm"
%use (web-make-communicator) "./web-make-communicator.scm"
%use (web-respond) "./web-respond.scm"
%use (web-set-cookie-header) "./web-set-cookie-header.scm"

%for (COMPILER "guile")
(use-modules (ice-9 iconv))
%end

(define (web-logincont)
  (define body/bytes (callcontext-body (web-callcontext/p)))

  (define _4
    (unless body/bytes
      (web-body-not-found)))

  (define body
    (bytevector->string body/bytes "utf-8"))

  (define parts
    (string-split/simple body #\&))

  (define key-values
    (map (fn string-split/simple % #\=) parts))

  (define _2
    (unless (list-singleton? key-values)
      (raisu 'too-many-query-parameters key-values)))

  (define-tuple (key password)
    (car key-values))

  (define _3
    (unless (equal? "psw" key)
      (raisu 'bad-query-key key)))

  (define result
    (profune-communicator-handle
     (web-make-communicator (web-context/p))
     `(whats
       (login ,password)
       (key K)
       )))

  (case (car result)
    ((its)
     (let* ((word (cadr result))
            (token (list-ref word 2)))
       (web-respond
        web-login-success-body
        #:extra-headers (list (web-set-cookie-header "pwdtoken" token)))))
    (else
     (web-respond web-login-failed-body))))
