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

%var standalone-file->entry
%var standalone-file->entry/prefixed

%use (path-get-basename) "./euphrates/path-get-basename.scm"
%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (path-normalize) "./euphrates/path-normalize.scm"

%use (entry-parent-directory-key) "./entry-parent-directory-key.scm"
%use (entry-parent-directory-vid-key) "./entry-parent-directory-vid-key.scm"

(define (standalone-file->entry filepath)
  (define dir (dirname filepath))
  (define name (path-get-basename filepath))
  `((id . ,filepath)
    (target . ,name)
    (,entry-parent-directory-key . ,dir)
    ))

(define (standalone-file->entry/prefixed prefix vid filepath)
  (define dir prefix)
  (define name filepath)
  (define full (path-normalize (append-posix-path dir name)))
  `((id . ,full)
    (target . ,name)
    (,entry-parent-directory-key . ,dir)
    (,entry-parent-directory-vid-key . ,vid)
    ))
