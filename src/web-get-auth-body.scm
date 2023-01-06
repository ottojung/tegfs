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

%var web::get-auth-body

%use (stringf) "./euphrates/stringf.scm"
%use (uri-encode) "./euphrates/uri-encode.scm"
%use (web::form-template) "./web-form-template.scm"

(define-syntax web::auth-encode-arg
  (syntax-rules ()
    ((_ name)
     (if (not name) ""
         (let* ((q (quote name))
                (s (symbol->string q)))
           (string-append
            s "=" (uri-encode name) "&"))))))

(define (web::get-auth-body failed? yes no expected)
  (define top
    (if failed?
        "<label><b>Bad password</b></label>"
        ""))

  (web::form-template
   (stringf "action='authcont?~a~a~a' enctype='application/x-www-form-urlencoded'"
            (web::auth-encode-arg yes)
            (web::auth-encode-arg no)
            (web::auth-encode-arg expected))
   (stringf
    "
    ~a
    <input type='password' placeholder='Enter Password' name='psw' required autofocus>
    <button type='submit'>Login</button>"
    top)))
