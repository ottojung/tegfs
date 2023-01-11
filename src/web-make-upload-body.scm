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
%use (web::form-template) "./web-form-template.scm"

(define (web::make-upload-body categorization-text)
  (define inner
    (with-output-to-string
      (lambda _
        (printf "
    <input type='file' name='file' autofocus>
    <textarea style='maxwidth: 100%; width: 100%;' rows='10' cols='120' name='tags'>~a</textarea>
    <input type='text' placeholder='Enter title' name='title' >
    <button type='submit'>Upload</button>"
                categorization-text))))

  (web::form-template "action='uploadcont' enctype='multipart/form-data'" inner))
