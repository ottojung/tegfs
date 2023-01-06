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

%var web::auth

%use (hashmap-ref) "./euphrates/hashmap.scm"
%use (stringf) "./euphrates/stringf.scm"
%use (uri-encode) "./euphrates/uri-encode.scm"
%use (web::callcontext/p) "./web-callcontext-p.scm"
%use (callcontext-query callcontext-token) "./web-callcontext.scm"
%use (web::form-template) "./web-form-template.scm"
%use (web::make-html-response) "./web-make-html-response.scm"

(define-syntax web::auth-encode-arg
  (syntax-rules ()
    ((_ name)
     (if (not name) ""
         (let* ((q (quote name))
                (s (symbol->string q)))
           (string-append
            s "=" (uri-encode name) "&"))))))

(define (web::get-auth-body yes no expected)
  (web::form-template
   (stringf "action='authcont?~a~a~a' enctype='application/x-www-form-urlencoded'"
            (web::auth-encode-arg yes)
            (web::auth-encode-arg no)
            (web::auth-encode-arg expected))
   "
    <input type='password' placeholder='Enter Password' name='psw' required autofocus>
    <button type='submit'>Login</button>"))

(define (web::auth)
  (define callctx (web::callcontext/p))
  (define key (callcontext-token callctx))
  (define query (callcontext-query callctx))
  (define yes-continue (hashmap-ref query 'yes #f))
  (define no-continue (hashmap-ref query 'no #f))
  (define expected-key (hashmap-ref query 'expected #f))

  (web::make-html-response
   (web::get-auth-body yes-continue no-continue expected-key)))
