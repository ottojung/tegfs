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
%var entries-for-each*
%var entries-for-each/no-registry-file

%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (open-file-port) "./euphrates/open-file-port.scm"
%use (read-list) "./euphrates/read-list.scm"

%use (get-root) "./get-root.scm"
%use (get-registry-files) "./get-registry-files.scm"
%use (entry-registry-path-key) "./entry-registry-path-key.scm"

(define (entries-for-each* append-registry-file? fn)
  (for-each
   (lambda (registry-path0)
     (define registry-path (append-posix-path registry-path0))
     (define registry-fullpath (append-posix-path (get-root) registry-path))
     (define input-port (open-file-port registry-fullpath "r"))
     (let loop ()
       (define x (read input-port))
       (unless (eof-object? x)
         (if append-registry-file?
             (fn (cons (cons entry-registry-path-key registry-path) x))
             (fn x))
         (loop))))
   (get-registry-files)))

(define (entries-for-each fn)
  (entries-for-each* #t fn))

(define (entries-for-each/no-registry-file fn)
  (entries-for-each* #f fn))
