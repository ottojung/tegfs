;;;; Copyright (C) 2022, 2023  Otto Jung
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

%var entries-map!

%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (file-delete) "./euphrates/file-delete.scm"
%use (open-file-port) "./euphrates/open-file-port.scm"
%use (entry-print) "./entry-print.scm"
%use (get-registry-files) "./get-registry-files.scm"
%use (get-root) "./get-root.scm"
%use (make-temporary-filename/local) "./make-temporary-filename-local.scm"

(define (entries-map! fun)
  (for-each
   (lambda (registry-path)
     (define registry-fullpath (append-posix-path (get-root) registry-path))
     (define input-port (open-file-port registry-fullpath "r"))
     (define temp-path (make-temporary-filename/local))
     (define temp-port (open-file-port temp-path "w"))
     (parameterize ((current-output-port temp-port))
       (display ";; This file was modified by `entries-map!'\n")
       (let loop ()
         (define x (read input-port))
         (unless (eof-object? x)
           (let ((ret (fun registry-path x)))
             (when ret
               (newline)
               (entry-print ret)
               (newline)))
           (loop))))
     (close-port temp-port)
     (file-delete registry-fullpath)
     (rename-file temp-path registry-fullpath))
   (get-registry-files)))
