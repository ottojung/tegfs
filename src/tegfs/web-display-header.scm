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
  (define-module (tegfs web-display-header)
    :export (web::display-header)
    :use-module ((tegfs web-callcontext) :select (callcontext-headers callcontext-token))
    :use-module ((tegfs web-get-cookie) :select (web::get-cookie))
    :use-module ((tegfs webcore-ask) :select (webcore::ask))
    )))



(define (web::display-header callctx)
  (define token (callcontext-token callctx))
  (define result
    (webcore::ask `(whats (time-left ,token _TL))))
  (define loged-in?
    (equal? 'its (car result)))

  (define headers (callcontext-headers callctx))
  (define usertype
    (cond
     ((web::get-cookie "key" headers) 'Anonymous)
     ((web::get-cookie "pwdtoken" headers) 'Admin)
     (else #f)))

  (define logout-link
    "<a class='highlighted last' href='logout'><li>Logout</li></a>")
  (define login-link
    "<a class='highlighted last' href='login'><li>Login</li></a>")

  (display
   "<header>
    <nav>
      <ul>
        <a href='home'><img src='static/logo-gray.jpeg'/></a>
        <a class='highlighted first' href='query'><li>Search</li></a>
        <a class='highlighted' href='upload'><li>Upload</li></a>
        <a class='highlighted' href='tags'><li>Tags</li></a>
    ")

  (display
   (if loged-in?
       (case usertype
         ((Admin Anonymous)
          logout-link)
         (else
          login-link))
       login-link))

  (display "\n</ul>\n")

  (case usertype
    ((Admin Anonymous)
     (display "
      <div id='lst'>
        <div>")
     (if loged-in?
         (begin
           (display "(Logged in as ")
           (display usertype)
           (display ")"))
         (display "(Token expired)"))
     (display "</div></div>")))

  (display "\n</nav></header>\n"))
