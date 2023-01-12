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

%var web::make-upload-body

%use (printf) "./euphrates/printf.scm"
%use (stringf) "./euphrates/stringf.scm"
%use (words->string) "./euphrates/words-to-string.scm"
%use (categorization-get-all-tags) "./categorization-get-all-tags.scm"
%use (web::form-template) "./web-form-template.scm"

(define (web::make-upload-body categorization-text)
  (define all-tags
    (categorization-get-all-tags categorization-text))

  (define (tag->checkbox tag)
    (stringf
     "<input type='checkbox' id='tag:~a' name='tag:~a' />
      <label for='tag:~a'>~a</label>"
     tag tag tag tag))

  (define checkboxes
    (words->string
     (map tag->checkbox all-tags)))

  (define inner
    (with-output-to-string
      (lambda _
        (printf "
    <input type='file' name='file' autofocus>
    <br/>
    <br/>
    <div class='form-block form-v-element'>
      <label>Tags</label>
    </div>
    <div class='tagsbox'>
      <div>~a</div>
    </div>
    <div class='form-block form-v-element'>
      <input type='text' name='additional-tags' placeholder='Additional tags' />
    </div>
    <input type='text' placeholder='Title' name='title' >
    <br/>
    <br/>
    <div class='form-block form-v-element'>
      <button type='submit'>Upload</button>
    </div>
    "
                checkboxes))))

  (web::form-template "action='upload?continue=on' oenctype='multipart/form-data'" inner))
