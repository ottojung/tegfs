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

%var get-file-mimetype

%use (string-strip) "./euphrates/string-strip.scm"
%use (system-re) "./euphrates/system-re.scm"

(define (get-file-mimetype target)
  (let* ((ret (system-re "file --brief --mime-type ~a" target))
         (mimetype (car ret))
         (code (cdr ret)))
    (if (= 0 code)
        (string-strip mimetype)
        #f)))
