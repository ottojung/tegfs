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

%var entries-for-each

%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (fn) "./euphrates/fn.scm"
%use (open-file-port) "./euphrates/open-file-port.scm"
%use (keyword-entry-registry-path) "./keyword-entry-registry-path.scm"
%use (get-registry-files) "./get-registry-files.scm"
%use (get-root) "./get-root.scm"

(define (entries-for-each fn)
  (for-each
   (lambda (registry-path0)
     (define registry-path (append-posix-path registry-path0))
     (define registry-fullpath (append-posix-path (get-root) registry-path))
     (define input-port (open-file-port registry-fullpath "r"))
     (let loop ()
       (define x (read input-port))
       (unless (eof-object? x)
         (fn (cons (cons keyword-entry-registry-path registry-path) x))
         (loop))))
   (get-registry-files)))
