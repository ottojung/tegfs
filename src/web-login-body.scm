;;;; Copyright (C) 2022  Otto Jung
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

%var web::login-body

%use (web::form-template) "./web-form-template.scm"

(define web::login-body
  (web::form-template "action='logincont' enctype='application/x-www-form-urlencoded'" "
    <div class='tiled-v-element split-container with-separator'>
      <div class='form-block'>
        <input type='password' placeholder='Enter Password' name='psw' required autofocus>
      </div>
    </div>
    <div class='form-block tiled-v-element'>
      <button type='submit'>Login</button>
    </div>
"))
