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

%var standalone-file->entry
%var standalone-file->entry/prefixed

%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (path-get-basename) "./euphrates/path-get-basename.scm"
%use (path-normalize) "./euphrates/path-normalize.scm"
%use (keyword-entry-parent-directory-senderid) "./keyword-entry-parent-directory-senderid.scm"
%use (keyword-entry-parent-directory) "./keyword-entry-parent-directory.scm"

(define (standalone-file->entry filepath)
  (define norm (path-normalize filepath))
  (define dir (dirname norm))
  (define name (path-get-basename norm))
  (define id norm)
  `((id . ,id)
    (target . ,name)
    (,keyword-entry-parent-directory . ,dir)
    ))

(define (standalone-file->entry/prefixed prefix vid filepath)
  (define norm (path-normalize filepath))
  (define dir prefix)
  (define name norm)
  (define id (path-normalize (append-posix-path dir norm)))
  `((id . ,id)
    (target . ,name)
    (,keyword-entry-parent-directory . ,dir)
    (,keyword-entry-parent-directory-senderid . ,vid)
    ))
