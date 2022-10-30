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

%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (path-get-basename) "./euphrates/path-get-basename.scm"
%use (path-normalize) "./euphrates/path-normalize.scm"
%use (keyword-entry-parent-directory-vid) "./keyword-entry-parent-directory-vid.scm"
%use (keyword-entry-parent-directory) "./keyword-entry-parent-directory.scm"

(define (standalone-file->entry filepath)
  (define norm (path-normalize filepath))
  (define dir (dirname norm))
  (define name (path-get-basename norm))
  `((id . ,norm)
    (target . ,name)
    (,keyword-entry-parent-directory . ,dir)
    ))

(define (standalone-file->entry/prefixed prefix vid filepath)
  (define dir prefix)
  (define norm (path-normalize filepath))
  (define name norm)
  (define full (path-normalize (append-posix-path dir norm)))
  `((id . ,full)
    (target . ,name)
    (,keyword-entry-parent-directory . ,dir)
    (,keyword-entry-parent-directory-vid . ,vid)
    ))
