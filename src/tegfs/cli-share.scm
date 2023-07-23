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
  (define-module (tegfs cli-share)
    :export (CLI::share CLI::share/parse)
    :use-module ((euphrates assq-or) :select (assq-or))
    :use-module ((euphrates list-and-map) :select (list-and-map))
    :use-module ((euphrates list-singleton-q) :select (list-singleton?))
    :use-module ((euphrates read-string-file) :select (read-string-file))
    :use-module ((euphrates run-syncproc-re-star) :select (run-syncproc/re*))
    :use-module ((euphrates string-to-seconds) :select (string->seconds))
    :use-module ((euphrates stringf) :select (stringf))
    :use-module ((euphrates tilda-a) :select (~a))
    :use-module ((euphrates un-tilda-s) :select (un~s))
    :use-module ((tegfs a-weblink-q) :select (a-weblink?))
    :use-module ((tegfs config-get) :select (config-get))
    :use-module ((tegfs fatal) :select (fatal))
    :use-module ((tegfs get-config) :select (get-config))
    :use-module ((tegfs keyword-config-fileserver) :select (keyword-config-fileserver))
    :use-module ((tegfs keyword-config-port) :select (keyword-config-port))
    :use-module ((tegfs web-current-fileserver-p) :select (web::current-fileserver/p))
    :use-module ((tegfs web-default-port) :select (web::default-port))
    :use-module ((tegfs web-get-server-operator-key-file) :select (web::get-server-operator-key-file))
    :use-module ((tegfs web-get-target-link) :select (web::get-target-link))
    )
  ))

(define (CLI::share <share-duration> <entry-id>)
  (define config (get-config))

  (define port
    (config-get
     (list keyword-config-port) config
     web::default-port))

  (define fileserver
    (config-get
     (list keyword-config-fileserver) config
     (fatal "Expected ~s to be set in the config file" (~a keyword-config-fileserver))))

  (define key
    (read-string-file
     (web::get-server-operator-key-file)))

  (define seconds
    (string->seconds <share-duration>))

  (define stc
    (stringf
     "entry:_E+entry-field:_E,\"id\",~s+share-full:_E,~a,_ATF,_F+link-shared:_F,FL"
     <entry-id> seconds
     ))

  (define-values (html status0)
    (run-syncproc/re* "wget" "--quiet"
                      (stringf "http://localhost:~a/api?key=~a&stc=~a"
                               port key stc)
                      "-O" "-"))

  (unless (= 0 status0)
    (fatal "Could not submit share request to a running server. Maybe the server is not running?"))

  (define answer
    (un~s html))

  (unless (and (list? answer)
               (list-singleton? answer))
    (fatal "Expected a singleton list as a response from server, got: ~s" answer))

  (define assignment
    (car answer))

  (when (null? assignment)
    (fatal "Expected a non-empty assignment as a response from server, got an empty one"))

  (unless (list-and-map pair? assignment)
    (fatal "Expected an assignment as a response from server, got: ~s" assignment))

  (define linkpath
    (assq-or 'FL assignment
             (fatal "Expected a value for variable ~s, got nothing: ~s" 'FL assignment)))

  (define target/0
    (parameterize ((web::current-fileserver/p fileserver))
      (web::get-target-link linkpath)))

  (define target
    (if (a-weblink? target/0)
        target/0
        (stringf "http://localhost:~a/~a" port target/0)))

  target)


(define (CLI::share/parse <share-duration> <entry-id>)
  (define target (CLI::share <share-duration> <entry-id>))
  (display target)
  (newline)
  (when #f #t))
