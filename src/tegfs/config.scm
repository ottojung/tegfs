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
    :use-module ((euphrates assq-or) :select (assq-or))
    :use-module ((euphrates dprintln) :select (dprintln))
    :use-module ((euphrates list-find-first) :select (list-find-first))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((tegfs fatal) :select (fatal))
    :use-module ((tegfs get-config) :select (get-config))
    :use-module ((tegfs set-config) :select (set-config))
    )))


;; TODO: Handle config file format errors
(define (tegfs-config/parse --display --write get set <name> <value> get-user set-user <user-name> <user-field> <user-value>)
  (define config (get-config))
  (define name (and <name> (string->symbol <name>)))
  (define user-field (and <user-field> (string->symbol <user-field>)))
  (define value (and <value> (or (string->number <value>) <value>)))
  (define user-value (and <user-value> (or (string->number <user-value>) <user-value>)))

  (define out
    (if --write write display))

  (unless config
    (fatal "Could not parse the config"))

  (cond
   (get
    (let ((p (assoc name config)))
      (if p
          (out (cadr p))
          (display "Not set\n" (current-error-port)))))
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
                 (equal? <user-name> (car (assq-or 'name u (list #f)))))
               #f
               users))
            (if the-user
                (if user-field
                    (let ((f (assq-or user-field the-user #f)))
                      (if f
                          (out (car f))
                          (parameterize ((current-output-port (current-error-port)))
                            (dprintln "Field ~s is missing for the user named ~s" user-field <user-name>))))
                    (out the-user))
                (parameterize ((current-output-port (current-error-port)))
                  (dprintln "No user named ~s" <user-name>))))
          (display "No users\n" (current-error-port)))))
   (else
    (raisu 'Impossible-case-7236123))))
