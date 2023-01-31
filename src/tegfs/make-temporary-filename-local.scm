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
  (define-module (tegfs make-temporary-filename-local)
    :export (make-temporary-filename/local)
    :use-module ((euphrates append-posix-path) :select (append-posix-path))
    :use-module ((euphrates file-or-directory-exists-q) :select (file-or-directory-exists?))
    :use-module ((euphrates make-directories) :select (make-directories))
    :use-module ((tegfs get-random-basename) :select (get-random-basename))
    :use-module ((tegfs get-root) :select (get-root))
    )))



(define (make-temporary-filename/local)
  (define dir
    (append-posix-path (get-root) "tmp"))
  (unless (file-or-directory-exists? dir)
    (make-directories dir))
  (append-posix-path dir (get-random-basename)))

