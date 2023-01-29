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

%var get-config

%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (file-or-directory-exists?) "./euphrates/file-or-directory-exists-q.scm"
%use (open-file-port) "./euphrates/open-file-port.scm"
%use (read-list) "./euphrates/read-list.scm"
%use (get-root) "./get-root.scm"

(define (get-config)
  (define path (append-posix-path (get-root) "config.tegfs.lisp"))
  (and
   (file-or-directory-exists? path)
   (let* ((p (open-file-port path "r"))
          (alist (read-list p)))
     (close-port p)
     alist)))
