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

(cond-expand
 (guile
  (define-module (tegfs web-server)
    :export (tegfs-serve/parse)
    :use-module ((euphrates assq-or) :select (assq-or))
    :use-module ((euphrates comp) :select (comp))
    :use-module ((euphrates dprintln) :select (dprintln))
    :use-module ((euphrates make-directories) :select (make-directories))
    :use-module ((euphrates path-get-dirname) :select (path-get-dirname))
    :use-module ((euphrates write-string-file) :select (write-string-file))
    :use-module ((tegfs default-sharedir) :select (default-sharedir))
    :use-module ((tegfs get-config) :select (get-config/fatal))
    :use-module ((tegfs keyword-config-authorization) :select (keyword-config-authorization))
    :use-module ((tegfs keyword-config-fileserver) :select (keyword-config-fileserver))
    :use-module ((tegfs keyword-config-port) :select (keyword-config-port))
    :use-module ((tegfs keyword-config-sharedir) :select (keyword-config-sharedir))
    :use-module ((tegfs web-collectgarbage) :select (web::collectgarbage))
    :use-module ((tegfs web-current-fileserver-p) :select (web::current-fileserver/p))
    :use-module ((tegfs web-current-sharedir-p) :select (web::current-sharedir/p))
    :use-module ((tegfs web-default-port) :select (web::default-port))
    :use-module ((tegfs web-get-server-operator-key-file) :select (web::get-server-operator-key-file))
    :use-module ((tegfs web-make-callcontext) :select (web::make-callcontext))
    :use-module ((tegfs web-make-communicator) :select (web::make-communicator))
    :use-module ((tegfs web-server-current-handler-p) :select (web::server-current/p))
    :use-module ((tegfs web-server-handle) :select (web::server-handle))
    :use-module ((tegfs web-token-override-p) :select (web::token-override/p))
    :use-module ((tegfs webcore-current-communicator-p) :select (webcore::current-communicator/p))
    :use-module ((tegfs with-current-time) :select (with-current-time))
    )))



(cond-expand
 (guile
  (use-modules (web server))
  (use-modules (web request))
  (use-modules (web uri))
  ))

(define (log-request request)
  (define uri (request-uri request))
  (display "Got request: ") (display (uri-path uri))
  (let ((q (uri-query uri)))
    (when q (display "?") (display q)))
  (display "\n")
  )

(define (make-handler server-operator-key)
  (lambda (request body)
    (define callctx (web::make-callcontext request body))
    (log-request request)
    (with-current-time (web::server-handle server-operator-key callctx))))

(define (tegfs-serve/parse)
  (define config (get-config/fatal))
  (define sharedir (car (assq-or keyword-config-sharedir config `(,default-sharedir))))
  (define <fileserver> (car (assq-or keyword-config-fileserver config `(#f))))
  (define --no-authorization (equal? "no" (car (assq-or keyword-config-authorization config `("yes")))))
  (define port (car (assq-or keyword-config-port config `(,web::default-port))))
  (define-values (comm server-operator-key) (web::make-communicator))
  (define token-override (if --no-authorization server-operator-key #f))

  (make-directories (path-get-dirname (web::get-server-operator-key-file)))
  (write-string-file (web::get-server-operator-key-file) server-operator-key)

  (dprintln "Starting the server")
  (parameterize ((webcore::current-communicator/p comm)
                 (web::current-fileserver/p <fileserver>)
                 (web::current-sharedir/p sharedir)
                 (web::server-current/p (comp (web::server-handle server-operator-key)))
                 (web::token-override/p token-override))
    (dprintln "Collecting garbage left from the previous run...")
    (with-current-time (web::collectgarbage))
    (dprintln "Done")
    (dprintln "Listening on port ~s" port)
    (run-server (make-handler server-operator-key) 'http `(#:host "0.0.0.0" #:port ,port))))
