;;;; Copyright (C) 2022  Otto Jung
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

%var web-make-context

%use (file-or-directory-exists?) "./euphrates/file-or-directory-exists-q.scm"
%use (hashmap-set! make-hashmap) "./euphrates/hashmap.scm"
%use (make-directories) "./euphrates/make-directories.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (~a) "./euphrates/tilda-a.scm"
%use (default-web-port) "./default-web-port.scm"
%use (filemap-make/empty) "./filemap.scm"
%use (get-config) "./get-config.scm"
%use (keyword-config-port) "./keyword-config-port.scm"
%use (context-ctr) "./web-context.scm"

(define (web-make-context)
  (define passwords (make-hashmap))
  (define tokens (make-hashmap))

  (define filemap/2
    (filemap-make/empty))
  (define config
    (get-config))
  (define _1
    (unless config
      (raisu 'no-config-file
             "Config file needs to be present when starting the server")))
  (define users
    (cdr
     (or (assoc 'users config)
         (raisu 'no-config-file
                "Config file needs to be present when starting the server"))))
  (define fileserver
    (cadr
     (or (assoc 'fileserver config)
         (raisu 'no-fileserver "Variable 'fileserver is not set by the config"))))
  (define sharedir
    (cadr
     (or (assoc 'sharedir config)
         (raisu 'no-fileserver "Variable 'sharedir is not set by the config"))))
  (define port/string
    (cadr
     (or (assoc keyword-config-port config)
         (list (number->string default-web-port)))))
  (define port
    (or (string->number (~a port/string))
        (raisu 'port-is-not-a-number "Variable 'port must be a number" port/string)))

  (unless (file-or-directory-exists? sharedir)
    (make-directories sharedir))

  (for-each
   (lambda (user)
     (define pass
       (cadr
        (or (assoc 'pass user)
            (raisu 'no-user-pass
                   "A user does not have a password"))))
     (unless (string? pass)
       (raisu 'pass-is-no-string "User passord is not a string" pass))
     (hashmap-set! passwords pass #t))
   users)

  (unless (string? fileserver)
    (raisu 'fileserver-must-be-a-string fileserver))
  (unless (string? sharedir)
    (raisu 'sharedir-must-be-a-string sharedir))
  (unless (and (integer? port) (exact? port) (> port 0))
    (raisu 'port-must-be-a-natural-number port))

  (context-ctr passwords tokens port fileserver sharedir filemap/2))
