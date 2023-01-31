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
  (define-module (tegfs web-tags-make-page)
    :export (web::tags::make-page)
    :use-module ((euphrates printf) :select (printf))
    :use-module ((tegfs web-form-template) :select (web::form-template))
    )))



(define (web::tags::make-page continue? categorization-text)
  (define label
    (if continue?
        "Updated categorization"
        "Current categorization"))

  (define inner
    (with-output-to-string
      (lambda _
        (printf "
    <div class='form-block form-v-element'>
      <label for='categorization'>~a</label>
    </div>
    <br/>
    <div class='form-block form-v-element'>
      <textarea autofocus rows='20' cols='120' name='categorization'>~a</textarea>
    </div>
    <div class='form-block form-v-element'>
      <button type='submit'>Update</button>
    </div>
    "
                label
                categorization-text))))

  (web::form-template "action='tags?continue=on' enctype='multipart/form-data'" inner))
