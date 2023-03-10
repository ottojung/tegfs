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
  (define-module (tegfs web-get-cookie)
    :export (web::get-cookie)
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((euphrates string-split-3) :select (string-split-3))
    :use-module ((euphrates string-split-simple) :select (string-split/simple))
    :use-module ((euphrates string-strip) :select (string-strip))
    )))



(define (parse-cookies-string cookies/string)
  (define _aa
    (unless (string? cookies/string)
      (raisu 'bad-cookies-cdr cookies/string)))

  (define cookie-split-semicolon
    (string-split/simple cookies/string #\;))

  (define cookie-split
    (map
     (lambda (c)
       (define-values (key eq val) (string-split-3 #\= c))
       (unless eq
         (raisu 'bad-cookie-split cookies/string))
       (cons (string-strip key) val))
     cookie-split-semicolon))

  cookie-split)

(define (web::get-cookie name headers)
  (let* ((cookies-p (assoc 'cookie headers))
         (cookies/string (and (pair? cookies-p) (cdr cookies-p)))
         (cookies (and cookies/string (parse-cookies-string cookies/string)))
         (got0 (and cookies (assoc name cookies)))
         (got (and got0 (cdr got0))))
    (and got (not (string-null? got)) got)))
