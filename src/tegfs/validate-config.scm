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
  (define-module (tegfs validate-config)
    :export (validate-config)
    :use-module ((euphrates list-intersperse) :select (list-intersperse))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((euphrates stringf) :select (stringf))
    :use-module ((euphrates tilda-a) :select (~a))
    )))

(define (validate-config config)
  (define (error fmt . args)
    (raisu 'config-format-error (apply stringf (cons fmt args))))

  (unless (list? config)
    (error "Config is not a list"))

  (let loop ((config config) (pref '()))
    (for-each
     (lambda (p)
       (unless (pair? p)
         (error "Field ~s~a expected to be of key-value type"
                p (if (null? pref) ""
                      (string-append
                       " (at "
                       (apply string-append (list-intersperse "." (reverse pref)))
                       ")"))))

       (unless (symbol? (car p))
         (error "Name of the field ~s~a expected be a symbol"
                p
                (if (null? pref) ""
                    (string-append
                     " (at "
                     (apply string-append (list-intersperse "." (reverse pref)))
                     ")"))))

       (when (equal? 'user (car p))
         (unless (list? (cdr p))
           (error "Field ~s expected to be a list" (~a 'users)))

         (for-each
          (lambda (u)
            (unless (list? (cdr u))
              (error "One of the users turned out to not be a list"))
            (loop (cdr u) (cons "user" pref)))
          (cdr p))))

     config)))
