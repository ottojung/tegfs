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
  (define-module (tegfs web-make-context)
    :export (web::make-context)
    :use-module ((euphrates assq-or) :select (assq-or))
    :use-module ((euphrates file-or-directory-exists-q) :select (file-or-directory-exists?))
    :use-module ((euphrates hashmap) :select (hashmap-set! make-hashmap))
    :use-module ((euphrates make-directories) :select (make-directories))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((euphrates stringf) :select (stringf))
    :use-module ((euphrates tilda-a) :select (~a))
    :use-module ((tegfs config-get) :select (config-get))
    :use-module ((tegfs default-sharedir) :select (default-sharedir))
    :use-module ((tegfs get-config) :select (get-config))
    :use-module ((tegfs keyword-config-port) :select (keyword-config-port))
    :use-module ((tegfs keyword-config-sharedir) :select (keyword-config-sharedir))
    :use-module ((tegfs keyword-config-user) :select (keyword-config-user))
    :use-module ((tegfs web-default-port) :select (web::default-port))
    :use-module ((tegfs webcore-context) :select (context-ctr))
    :use-module ((tegfs webcore-credentials-to-id) :select (webcore::credentials->id))
    :use-module ((tegfs webcore-user) :select (webcore::user-make))
    )))



(define (web::make-context)
  (define users-map (make-hashmap))
  (define tempentries (make-hashmap))

  (define config (get-config))

  (define users
    (config-get
     (list keyword-config-user) config
     (raisu 'no-users
            (stringf "Variable ~s is not set in the config" (~a keyword-config-user)))))

  (define sharedir
    (config-get
     (list keyword-config-sharedir) config
     default-sharedir))

  (define port
    (config-get
     (list keyword-config-port) config
     web::default-port))

  (unless (number? port)
    (raisu 'port-is-not-a-number "Variable 'port must be a number" port))

  (unless (and (integer? port) (exact? port) (> port 0))
    (raisu 'port-must-be-a-natural-number port))

  (unless (file-or-directory-exists? sharedir)
    (make-directories sharedir))

  (unless (string? sharedir)
    (raisu 'sharedir-must-be-a-string sharedir))

  (for-each
   (lambda (user-tuple)
     (define name #f) ;; TODO: actually this: (~a (car user-tuple))), but currently not supporting many users.
     (define user (cdr user-tuple))

     (define pass
       (~a
        (assq-or
         'pass user
         (raisu 'no-user-pass
                "A user does not have a password"))))

     (define struct (webcore::user-make name pass))
     (define id (webcore::credentials->id name pass))
     (hashmap-set! users-map id struct))
   users)

  (context-ctr users-map tempentries port sharedir))
