;;;; Copyright (C) 2022  Otto Jung
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; version 3 of the License.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

%run guile

%var web-make-upload-body

%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (memconst) "./euphrates/memconst.scm"
%use (printf) "./euphrates/printf.scm"
%use (read-string-file) "./euphrates/read-string-file.scm"
%use (categorization-filename) "./categorization-filename.scm"
%use (get-root) "./get-root.scm"
%use (web-form-template) "./web-form-template.scm"

(define web-make-upload-body
  (memconst
   (let ()
     (define categorization-file (append-posix-path (get-root) categorization-filename))
     (define tags-value (read-string-file categorization-file))

     (define inner
       (with-output-to-string
         (lambda _
           (printf "
    <input type='text' placeholder='Enter title' name='title' >
    <input type='file' name='file' >
    <textarea rows='10' cols='120' name='tags'>~a</textarea>
    <button type='submit'>Upload</button>"
                   tags-value))))

     (web-form-template "action='uploadcont' enctype='multipart/form-data'" inner))))
