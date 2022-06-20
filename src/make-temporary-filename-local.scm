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

%var make-temporary-filename/local

%use (make-directories) "./euphrates/make-directories.scm"
%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (file-or-directory-exists?) "./euphrates/file-or-directory-exists-q.scm"

%use (get-random-basename) "./get-random-basename.scm"
%use (get-root) "./get-root.scm"

(define (make-temporary-filename/local)
  (define dir
    (append-posix-path (get-root) "tmp"))
  (unless (file-or-directory-exists? dir)
    (make-directories dir))
  (append-posix-path dir (get-random-basename)))

