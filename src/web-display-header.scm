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

%use (printf) "./euphrates/printf.scm"
%use (stringf) "./euphrates/stringf.scm"
%use (callcontext-headers) "./web-callcontext.scm"
%use (web::get-cookie) "./web-get-cookie.scm"

(define (web::display-header callctx)
  (define headers (callcontext-headers callctx))
  (define usertype
    (cond
     ((web::get-cookie "key" headers) 'Anonymous)
     ((web::get-cookie "pwdtoken" headers) 'Admin)
     (else #f)))

  (define auth
    (case usertype
      ((Admin Anonymous)
       (stringf
        "<div>
           <a href='logout'>Logout</a>
           <label>(Loged in as <i>~a</i>)</label>
         </div>" usertype))
      (else "<a href='login'>Login</a>")))

  (printf
   "<nav>
      <ul>
        <li><a href='query'>Search</a></li>
        <li><a href='upload'>Upload</a></li>
        <li><a href='tags'>Tags</a></li>
        <li>~a</li>
      </ul>
    </nav>"
   auth))
