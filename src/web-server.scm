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

%var tegfs-serve/parse

%use (assq-or) "./euphrates/assq-or.scm"
%use (dprintln) "./euphrates/dprintln.scm"
%use (default-web-port) "./default-web-port.scm"
%use (get-config) "./get-config.scm"
%use (keyword-config-port) "./keyword-config-port.scm"
%use (web::current-temp-paths-table/p) "./web-current-temp-paths-table-p.scm"
%use (web-make-callcontext) "./web-make-callcontext.scm"
%use (web-make-communicator) "./web-make-communicator.scm"
%use (web::make-temp-paths-table) "./web-make-temp-paths-table.scm"
%use (web::server-current-handler/p) "./web-server-current-handler-p.scm"
%use (web::server-handle) "./web-server-handle.scm"
%use (webcore::ask) "./webcore-ask.scm"
%use (webcore::current-communicator/p) "./webcore-current-communicator-p.scm"
%use (with-current-time) "./with-current-time.scm"

%for (COMPILER "guile")
(use-modules (web server))
(use-modules (web request))
(use-modules (web uri))
%end

(define (log-request request)
  (define uri (request-uri request))
  (display "Got request: ") (display (uri-path uri))
  (let ((q (uri-query uri)))
    (when q (display "?") (display q)))
  (display "\n")
  )

(define (make-handler)
  (lambda (request body)
    (define callctx (web-make-callcontext request body))
    (log-request request)
    (with-current-time (web::server-handle callctx))))

(define (tegfs-serve/parse)
  (define config (get-config))
  (define port (car (assq-or keyword-config-port config '(,default-web-port))))
  (define comm (web-make-communicator))
  (define tptable (web::make-temp-paths-table))
  (define handler (make-handler))

  (dprintln "Starting the server")
  (parameterize ((webcore::current-communicator/p comm)
                 (web::current-temp-paths-table/p tptable)
                 (web::server-current-handler/p handler))
    (dprintln "Collecting garbage left from the previous run...")
    (with-current-time
     (webcore::ask `(whats (collectgarbage))))
    (dprintln "Done")
    (dprintln "Listening on port ~s" port)
    (run-server handler 'http `(#:port ,port))))
