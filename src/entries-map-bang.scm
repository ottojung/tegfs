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

%var entries-map!

%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (open-file-port) "./euphrates/open-file-port.scm"
%use (read-list) "./euphrates/read-list.scm"
%use (file-delete) "./euphrates/file-delete.scm"

%use (get-root) "./get-root.scm"
%use (get-registry-files) "./get-registry-files.scm"
%use (entry-registry-path-key) "./entry-registry-path-key.scm"
%use (entry-print) "./entry-print.scm"
%use (make-temporary-filename/local) "./make-temporary-filename-local.scm"

(define (entries-map! fn)
  (for-each
   (lambda (registry-path)
     (define registry-fullpath (append-posix-path (get-root) registry-path))
     (define input-port (open-file-port registry-fullpath "r"))
     (define temp-path (make-temporary-filename/local))
     (define temp-port (open-file-port temp-path "w"))
     (parameterize ((current-output-port temp-port))
       (display ";; This file was modified by `entries-map!'\n\n")
       (let loop ()
         (define x (read input-port))
         (unless (eof-object? x)
           (let ((ret (fn x)))
             (when ret
               (entry-print ret)
               (display "\n\n")))
           (loop))))
     (close-port temp-port)
     (file-delete registry-fullpath)
     (rename-file temp-path registry-fullpath))
   (get-registry-files)))
