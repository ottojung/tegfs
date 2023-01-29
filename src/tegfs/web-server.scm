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
    :use-module ((euphrates dprintln) :select (dprintln))
    :use-module ((tegfs get-config) :select (get-config))
    :use-module ((tegfs keyword-config-port) :select (keyword-config-port))
    :use-module ((tegfs web-collectgarbage) :select (web::collectgarbage))
    :use-module ((tegfs web-current-temp-paths-table-p) :select (web::current-temp-paths-table/p))
    :use-module ((tegfs web-default-port) :select (web::default-port))
    :use-module ((tegfs web-make-callcontext) :select (web::make-callcontext))
    :use-module ((tegfs web-make-communicator) :select (web::make-communicator))
    :use-module ((tegfs web-make-temp-paths-table) :select (web::make-temp-paths-table))
    :use-module ((tegfs web-server-current-handler-p) :select (webcore::server-current/p))
    :use-module ((tegfs web-server-handle) :select (web::server-handle))
    :use-module ((tegfs webcore-current-communicator-p) :select (webcore::current-communicator/p))
    :use-module ((tegfs with-current-time) :select (with-current-time)))))



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

(define (make-handler)
  (lambda (request body)
    (define callctx (web::make-callcontext request body))
    (log-request request)
    (with-current-time (web::server-handle callctx))))

(define (tegfs-serve/parse)
  (define config (get-config))
  (define port (car (assq-or keyword-config-port config '(,web::default-port))))
  (define-values (comm server-operator-key) (web::make-communicator))
  (define tptable (web::make-temp-paths-table))

  (dprintln "Starting the server")
  (parameterize ((webcore::current-communicator/p comm)
                 (web::current-temp-paths-table/p tptable)
                 (webcore::server-current/p web::server-handle))
    (dprintln "Collecting garbage left from the previous run...")
    (with-current-time (web::collectgarbage))
    (dprintln "Done")
    (dprintln "Listening on port ~s" port)
    (run-server (make-handler) 'http `(#:port ,port))))
