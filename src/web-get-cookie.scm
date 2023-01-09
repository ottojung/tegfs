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

%run guile

%var web::get-cookie

%use (raisu) "./euphrates/raisu.scm"
%use (string-split-3) "./euphrates/string-split-3.scm"
%use (string-split/simple) "./euphrates/string-split-simple.scm"
%use (string-strip) "./euphrates/string-strip.scm"

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
         (got (and cookies (assoc name cookies))))
    (and got (cdr got))))
