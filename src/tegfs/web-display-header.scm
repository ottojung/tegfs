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
    :use-module ((tegfs web-callcontext) :select (callcontext-headers))
    :use-module ((tegfs web-get-cookie) :select (web::get-cookie))
    :use-module ((tegfs web-token-override-p) :select (web::token-override/p))
    :use-module ((tegfs web-user-loged-in-q) :select (web::user-loged-in?))
    )))



(define (web::display-header callctx)
  (define authoriation-disabled?
    (web::token-override/p))
  (define loged-in?
    (or authoriation-disabled?
        (web::user-loged-in? callctx)))

  (define headers (callcontext-headers callctx))
  (define usertype
    (cond
     ((web::get-cookie "key" headers) 'Anonymous)
     ((web::get-cookie "pwdtoken" headers) 'Admin)
     (else #f)))

  (display
   "
<header>
  <nav>
    <a href='home'><img src='static/logo-gray.jpeg'/></a>
    <ul>
      <a class='highlighted first' href='query'><li>Search</li></a>
      <a class='highlighted' href='upload'><li>Upload</li></a>
      <a class='highlighted")

  (when authoriation-disabled?
    (display " last"))

  (display "' href='tags'><li>Tags</li></a>\n")

  (define logout-link
    "<a class='highlighted last' href='logout'><li>Logout</li></a>")
  (define login-link
    "<a class='highlighted last' href='login'><li>Login</li></a>")

  (unless authoriation-disabled?
    (display
     (if loged-in?
         (case usertype
           ((Admin Anonymous)
            logout-link)
           (else
            login-link))
         login-link)))

  (unless authoriation-disabled?
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
       (display "    </div>
      </div>"))))

  (display "
    </ul>
  </nav>
</header>

"))
