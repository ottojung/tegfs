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

%var web::share::get-settings-text

%use (hashmap-set!) "./euphrates/hashmap.scm"
%use (stringf) "./euphrates/stringf.scm"
%use (callcontext-query) "./web-callcontext.scm"
%use (web::form-template) "./web-form-template.scm"
%use (web::hashmap->query) "./web-hashmap-to-query.scm"

(define web::share::settings-template
  (web::form-template
   "action='share?~a' method='post'"
   "<div class='tiled-v-element'>
      <div class='form-block'>
        <label for='for-duration'>Duration</label>
        <input placeholder='Ex: \"2d5h\" for 2 days and 5 hours' name='for-duration' required autofocus>
      </div>
    </div>
    <br/>
    <div class='form-block tiled-v-element'>
      <button type='submit'>Share</button>
    </div>
"))

(define (web::share::get-settings-text callctx)
  (define query
    (let ((original (callcontext-query callctx)))
      (hashmap-set! original 'settings 'no)
      original))

  (define query/s
    (web::hashmap->query query))

  (stringf web::share::settings-template
           query/s))
