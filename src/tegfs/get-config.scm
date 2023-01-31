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

(cond-expand
 (guile
  (define-module (tegfs get-config)
    :export (get-config)
    :use-module ((euphrates append-posix-path) :select (append-posix-path))
    :use-module ((euphrates file-or-directory-exists-q) :select (file-or-directory-exists?))
    :use-module ((euphrates open-file-port) :select (open-file-port))
    :use-module ((euphrates read-list) :select (read-list))
    :use-module ((tegfs get-root) :select (get-root))
    )))



(define (get-config)
  (define path (append-posix-path (get-root) "config.tegfs.lisp"))
  (and
   (file-or-directory-exists? path)
   (let* ((p (open-file-port path "r"))
          (alist (read-list p)))
     (close-port p)
     alist)))
