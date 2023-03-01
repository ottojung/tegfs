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
  (define-module (tegfs config)
    :export (tegfs-config/parse)
    :use-module ((euphrates list-find-first) :select (list-find-first))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((tegfs fatal) :select (fatal))
    :use-module ((tegfs get-config) :select (get-config/fatal))
    :use-module ((tegfs set-config-user) :select (set-config-user))
    :use-module ((tegfs set-config) :select (set-config))
    :use-module ((tegfs sha256sum) :select (sha256sum))
    )))


(define (tegfs-config/parse --display --write get set <name> <value> get-user set-user <user-name> <user-field> --password <user-value>)
  (define config (get-config/fatal))
  (define name (and <name> (string->symbol <name>)))
  (define user-field
    (if --password 'pass
        (and <user-field> (string->symbol <user-field>))))
  (define value (and <value> (or (string->number <value>) <value>)))
  (define user-value
    (if --password
        (sha256sum <user-value>)
        (and <user-value> (or (string->number <user-value>) <user-value>))))

  (define (out x)
    (if --write (write x) (display x))
    (newline))

  (unless config
    (fatal "Could not parse the config"))

  (cond
   (get
    (let ((p (assoc name config)))
      (if p
          (out (cadr p))
          (fatal "Not set"))))

   (set
    (set-config name (list value))
    (display "Ok\n" (current-error-port)))

   (get-user
    (let ((p (assoc 'users config)))
      (if p
          (let ((users (cadr p)))
            (define the-user
              (list-find-first
               (lambda (u)
                 (define name (assoc 'name u))
                 (and name (equal? <user-name>  (cadr name))))
               #f
               users))
            (if the-user
                (if user-field
                    (let ((f (assoc user-field the-user)))
                      (if f
                          (out (cadr f))
                          (fatal "Field ~s is missing for the user named ~s" user-field <user-name>)))
                    (out the-user))
                (fatal "No user named ~s" <user-name>)))
          (fatal "No users"))))

   (set-user
    (set-config-user <user-name> user-field user-value)
    (display "Ok\n" (current-error-port)))

   (else
    (raisu 'Impossible-case-7236123))))
