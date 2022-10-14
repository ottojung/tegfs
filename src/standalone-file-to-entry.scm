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

%use (absolute-posix-path?) "./euphrates/absolute-posix-path-q.scm"
%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (path-get-basename) "./euphrates/path-get-basename.scm"
%use (path-normalize) "./euphrates/path-normalize.scm"
%use (entry-parent-directory-key) "./entry-parent-directory-key.scm"
%use (entry-parent-directory-vid-key) "./entry-parent-directory-vid-key.scm"

(define (standalone-file->entry filepath tags)
  (define norm (path-normalize filepath))
  (define dir (dirname norm))
  (define name (string-append "/" (path-get-basename norm)))
  (define ret
    `((id . ,norm)
      (target . ,name)
      (,entry-parent-directory-key . ,dir)
      ))

  (if tags (cons (cons 'tags tags) ret)
      ret))

(define (standalone-file->entry/prefixed prefix vid filepath tags)
  (define dir prefix)
  (define norm (path-normalize filepath))
  (define name (if (absolute-posix-path? norm) norm
                   (string-append "/" norm)))
  (define full (path-normalize (append-posix-path dir norm)))
  (define ret
    `((id . ,full)
      (target . ,name)
      (,entry-parent-directory-key . ,dir)
      (,entry-parent-directory-vid-key . ,vid)
      ))

  (if tags (cons (cons 'tags tags) ret)
      ret))
