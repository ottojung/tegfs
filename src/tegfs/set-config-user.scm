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
  (define-module (tegfs set-config-user)
    :export (set-config-user)
    :use-module ((euphrates append-posix-path) :select (append-posix-path))
    :use-module ((euphrates assoc-set-value) :select (assoc-set-value))
    :use-module ((euphrates open-file-port) :select (open-file-port))
    :use-module ((tegfs get-config) :select (get-config))
    :use-module ((tegfs get-root) :select (get-root))
    )))


(define (transform-users <user-name> user-field value users)
  (define found? #f)
  (define new-users0
    (map
     (lambda (u)
       (define name (assoc 'name u))
       (if (and name (equal? <user-name>  (cadr name)))
           (begin
             (set! found? #t)
             (assoc-set-value user-field (list value) u))
           u))
     users))

  (define new-users
    (if found? new-users0
        (cons `((name ,<user-name>) (,user-field ,value))
              new-users0)))

  (write (cons 'users (list new-users)))
  (newline))

(define (set-config-user <user-name> user-field value)
  (define path (append-posix-path (get-root) "config.tegfs.lisp"))
  (define existing (or (get-config) '()))
  (define p (open-file-port path "w"))
  (define found? #f)
  (parameterize ((current-output-port p))
    (for-each
     (lambda (x)
       (if (equal? (car x) 'users)
           (begin
             (set! found? #t)
             (transform-users <user-name> user-field value (cadr x)))
           (begin (write x) (newline))))
     existing))
  (unless found?
    (write `(users (((name ,<user-name>) (,user-field ,value)))))
    (newline))

  (close-port p)
  (values))
