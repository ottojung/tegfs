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
%use (alist->hashmap hashmap-ref) "./euphrates/hashmap.scm"
%use (~a) "./euphrates/tilda-a.scm"
%use (default-web-port) "./default-web-port.scm"
%use (get-config) "./get-config.scm"
%use (keyword-config-port) "./keyword-config-port.scm"
%use (web-callcontext/p) "./web-callcontext-p.scm"
%use (web-collectgarbage) "./web-collectgarbage.scm"
%use (web::current-temp-paths-table/p) "./web-current-temp-paths-table-p.scm"
%use (web-details) "./web-details.scm"
%use (web-directory) "./web-directory.scm"
%use (web-full) "./web-full.scm"
%use (web-login) "./web-login.scm"
%use (web-logincont) "./web-logincont.scm"
%use (web-main.css) "./web-main-css.scm"
%use (web-make-callcontext) "./web-make-callcontext.scm"
%use (web-make-communicator) "./web-make-communicator.scm"
%use (web::make-temp-paths-table) "./web-make-temp-paths-table.scm"
%use (web-not-found) "./web-not-found.scm"
%use (web-previewunknown) "./web-previewunknown.scm"
%use (web-previewunknownurl) "./web-previewunknownurl.scm"
%use (web-query) "./web-query.scm"
%use (web-share) "./web-share.scm"
%use (web-upload) "./web-upload.scm"
%use (web-uploadcont) "./web-uploadcont.scm"
%use (webcore::ask) "./webcore-ask.scm"
%use (webcore::current-communicator/p) "./webcore-current-communicator-p.scm"
%use (with-current-time) "./with-current-time.scm"

%for (COMPILER "guile")

(use-modules (web server))
(use-modules (web request))
(use-modules (web uri))

%end

(define handlers-config
  `((/login ,web-login)
    (/logincont ,web-logincont)
    (/collectgarbage ,web-collectgarbage)
    (/query ,web-query)
    (/directory ,web-directory)
    (/details ,web-details)
    (/full ,web-full)
    (/upload ,web-upload)
    (/uploadcont ,web-uploadcont)
    (/share ,web-share)

    (/static/main.css ,web-main.css)
    (/static/previewunknown.svg ,web-previewunknown)
    (/static/previewunknownurl.svg ,web-previewunknownurl)
    ))

(define handlers-funcmap
  (alist->hashmap
   (map
    (lambda (p) (cons (~a (car p)) (cadr p)))
    handlers-config)))

(define (log-request request)
  (define uri (request-uri request))
  (display "Got request: ") (display (uri-path uri))
  (let ((q (uri-query uri)))
    (when q (display "?") (display q)))
  (display "\n")
  )

(define (handler request body)
  (define path (uri-path (request-uri request)))

  (let* ((target path)
         (func (hashmap-ref handlers-funcmap target #f)))
    (if func
        (func)
        (web-not-found))))

(define (make-handler)
  (lambda (request body)
    (log-request request)
    (parameterize ((web-callcontext/p (web-make-callcontext request body)))
      (with-current-time
       (handler request body)))))

(define (tegfs-serve/parse)
  (define config (get-config))
  (define port (car (assq-or keyword-config-port config '(,default-web-port))))
  (define comm (web-make-communicator))
  (define tptable (web::make-temp-paths-table))

  (dprintln "Starting the server")
  (parameterize ((webcore::current-communicator/p comm)
                 (web::current-temp-paths-table/p tptable))
    (dprintln "Collecting garbage left from the previous run...")
    (with-current-time
     (webcore::ask `(whats (collectgarbage))))
    (dprintln "Done")
    (dprintln "Listening on port ~s" port)
    (run-server (make-handler) 'http `(#:port ,port))))
