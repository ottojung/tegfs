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

(cond-expand
 (guile
  (define-module (tegfs get-file-mimetype)
    :export (get-file-mimetype)
    :use-module ((euphrates string-strip) :select (string-strip))
    :use-module ((euphrates system-re) :select (system-re))
    :use-module ((tegfs a-weblink-q) :select (a-weblink?))
    )))



(define (get-file-mimetype target)
  (if (a-weblink? target)
      "text/uri-list"
      (let* ((ret (system-re "file --brief --mime-type ~a" target))
             (mimetype (car ret))
             (code (cdr ret)))
        (if (= 0 code)
            (string-strip mimetype)
            #f))))
