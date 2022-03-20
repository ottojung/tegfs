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

%var unparse-tag

%use (list-intersperse) "./euphrates/list-intersperse.scm"

(define (unparse-tag parsed-tag)
  (if (null? (cdr parsed-tag))
      (car parsed-tag)
      (string->symbol
       (apply
        string-append
        `(,(symbol->string (car parsed-tag)) "="
          ,@(list-intersperse "," (map symbol->string (cdr parsed-tag))))))))
