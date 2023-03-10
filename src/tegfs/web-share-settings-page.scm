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
  (define-module (tegfs web-share-settings-page)
    :export (web::share::get-settings-text)
    :use-module ((euphrates hashmap) :select (hashmap-delete!))
    :use-module ((euphrates stringf) :select (stringf))
    :use-module ((tegfs web-callcontext) :select (callcontext-query))
    :use-module ((tegfs web-form-template) :select (web::form-template))
    :use-module ((tegfs web-hashmap-to-query) :select (web::hashmap->query))
    )))



(define (web::share::settings-template query/s)
  (web::form-template
   (stringf "action='share?~a' method='post'" query/s)
   "<div class='tiled-v-element'>
      <div class='form-block'>
        <label for='for-duration'>Duration</label>
        <input type='text' placeholder='Ex: \"2d5h\" for 2 days and 5 hours' name='for-duration' required autofocus>
      </div>
    </div>
    <div class='form-block tiled-v-element'>
      <button type='submit'>Share</button>
    </div>
"))

(define (web::share::get-settings-text callctx)
  (define query
    (let ((original (callcontext-query callctx)))
      (hashmap-delete! original 'settings)
      original))

  (define query/s
    (web::hashmap->query query))

  (web::share::settings-template query/s))
