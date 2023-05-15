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

(cond-expand
 (guile
  (define-module (tegfs web-make-context)
    :export (web::make-context)
    :use-module ((euphrates file-or-directory-exists-q) :select (file-or-directory-exists?))
    :use-module ((euphrates hashmap) :select (hashmap-set! make-hashmap))
    :use-module ((euphrates make-directories) :select (make-directories))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((euphrates tilda-a) :select (~a))
    :use-module ((tegfs default-sharedir) :select (default-sharedir))
    :use-module ((tegfs get-config) :select (get-config))
    :use-module ((tegfs keyword-config-port) :select (keyword-config-port))
    :use-module ((tegfs web-default-port) :select (web::default-port))
    :use-module ((tegfs webcore-context) :select (context-ctr))
    :use-module ((tegfs webcore-credentials-to-id) :select (webcore::credentials->id))
    :use-module ((tegfs webcore-user) :select (webcore::user-make))
    )))



(define (web::make-context)
  (define users-map (make-hashmap))
  (define tempentries (make-hashmap))

  (define config
    (get-config))
  (define _1
    (unless config
      (raisu 'no-config-file
             "Config file needs to be present when starting the server")))
  (define users
    (cadr
     (or (assoc 'users config)
         (raisu 'no-users "Variable 'users is not set by the config"))))
  (define sharedir
    (cadr
     (or (assoc 'sharedir config)
         (list 'sharedir default-sharedir))))
  (define port/string
    (cadr
     (or (assoc keyword-config-port config)
         (list keyword-config-port (number->string web::default-port)))))
  (define port
    (or (string->number (~a port/string))
        (raisu 'port-is-not-a-number "Variable 'port must be a number" port/string)))

  (unless (file-or-directory-exists? sharedir)
    (make-directories sharedir))

  (for-each
   (lambda (user)
     (define name
       #f)
     ;; TODO: actually have the name here
       ;; (cadr
       ;;  (or (assoc 'name user) (list #f #f))))
     (define pass
       (cadr
        (or (assoc 'pass user)
            (raisu 'no-user-pass
                   "A user does not have a password"))))
     (define struct (webcore::user-make name pass))
     (define id (webcore::credentials->id name pass))
     (unless (string? pass)
       (raisu 'pass-is-no-string "User password is not a string" pass))
     (hashmap-set! users-map id struct))
   users)

  (unless (string? sharedir)
    (raisu 'sharedir-must-be-a-string sharedir))
  (unless (and (integer? port) (exact? port) (> port 0))
    (raisu 'port-must-be-a-natural-number port))

  (context-ctr users-map tempentries port sharedir))
