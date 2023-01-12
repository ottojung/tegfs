;;;; Copyright (C) 2022, 2023  Otto Jung
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

%use (categorization-get-all-tags) "./categorization-get-all-tags.scm"
%use (web::form-template/wide) "./web-form-template-wide.scm"
%use (web::sxml->xml) "./web-sxml-to-xml.scm"

(define (web::make-upload-body categorization-text)
  (define all-tags
    (categorization-get-all-tags categorization-text))

  (define (tag->checkbox tag0)
    (define tag (symbol->string tag0))
    (web::sxml->xml
     `(input (@ (type "checkbox")
                (id ,(string-append "tag:" tag))
                (name ,(string-append "tag:" tag)))))
    (web::sxml->xml
     `(label (@ (for ,(string-append "tag:" tag)))
             ,tag)))

  (define (inner)
    (display "
    <input type='file' name='file' autofocus>
    <br/>
    <br/>
    <div class='form-block form-v-element'>
      <label>Tags</label>
    </div>
    <div class='tagsbox'>
      <div>")

    (for-each tag->checkbox all-tags)

    (display "</div>
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
    "))

  (web::form-template/wide "action='upload?continue=on' enctype='multipart/form-data'" inner))
