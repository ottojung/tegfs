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

%var get-registry-files

%use (remove-common-prefix) "./euphrates/remove-common-prefix.scm"
%use (directory-files-rec/filter) "./euphrates/directory-files-rec-filter.scm"

%use (get-root) "./get-root.scm"
%use (regfile-suffix) "./regfile-suffix.scm"

(define (get-registry-files)
  (map (lambda (path)
         (remove-common-prefix path (string-append (get-root) "/")))
       (map car
            (directory-files-rec/filter
             (lambda (fullname)
               (string-suffix? regfile-suffix (basename fullname)))
             (get-root)))))


