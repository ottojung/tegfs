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

%var get-preview-path

%use (append-posix-path) "./euphrates/append-posix-path.scm"

%use (get-file-type) "./get-file-type.scm"
%use (root/p) "./root-p.scm"

(define (get-preview-path target-id target-fullpath)
  (define preview-directory
    (append-posix-path (root/p) "cache" "preview"))
  (define file-type (get-file-type target-fullpath))
  (define preview-name
    (string-append
     target-id
     (case file-type
      ((image) ".jpeg")
      ((video) ".gif")
      (else "TODO???"))))
  (define preview-fullpath
    (append-posix-path preview-directory preview-name))

  preview-fullpath)
