;;;; Copyright (C) 2023  Otto Jung
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

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

  (define key-file
    (web::get-server-operator-key-file))
  (define key
    (if (file-or-directory-exists? key-file)
        (read-string-file key-file)
        (fatal "Server key does not exist, is there a server running? Cannot share files without it.")))

  (define seconds
    (string->seconds <share-duration>))

  (define stc
    (stringf
     "entry:_E+entry-field:_E,\"id\",~s+share-full:_E,~a,_ATF,_F+link-shared:_F,FL"
     <entry-id> seconds
     ))

  (define-values html
    (catchu-case
     (download-string
      '() (stringf "http://localhost:~a/api?key=~a&stc=~a"
                   port key stc))

     (('download-failed . args)
      (fatal "Could not submit share request to a running server. Maybe the server is not running?"))))

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
