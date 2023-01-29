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

%var web::display-header

%use (callcontext-headers) "./web-callcontext.scm"
%use (web::get-cookie) "./web-get-cookie.scm"

(define (web::display-header callctx)
  (define headers (callcontext-headers callctx))
  (define usertype
    (cond
     ((web::get-cookie "key" headers) 'Anonymous)
     ((web::get-cookie "pwdtoken" headers) 'Admin)
     (else #f)))

  (display
   "<header>
    <nav>
      <ul>
        <a href='home'><img src='static/logo-gray.jpeg'/></a>
        <a class='highlighted first' href='query'><li>Search</li></a>
        <a class='highlighted' href='upload'><li>Upload</li></a>
        <a class='highlighted' href='tags'><li>Tags</li></a>
    ")

  (case usertype
    ((Admin Anonymous)
     (display "<a class='highlighted last' href='logout'><li>Logout</li></a>"))
    (else
     (display "<a class='highlighted last' href='login'><li>Login</li></a>")))

  (display "\n</ul>\n")

  (case usertype
    ((Admin Anonymous)
     (display "
      <div id='lst'>
        <div>(Logged in as ")
     (display usertype)
     (display ")</div></div>")))

  (display "\n</nav></header>\n"))
