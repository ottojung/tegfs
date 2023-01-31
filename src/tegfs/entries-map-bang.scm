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

(cond-expand
 (guile
  (define-module (tegfs entries-map-bang)
    :export (entries-map!)
    :use-module ((euphrates append-posix-path) :select (append-posix-path))
    :use-module ((euphrates file-delete) :select (file-delete))
    :use-module ((euphrates open-file-port) :select (open-file-port))
    :use-module ((tegfs entry-print) :select (entry-print))
    :use-module ((tegfs get-registry-files) :select (get-registry-files))
    :use-module ((tegfs get-root) :select (get-root))
    :use-module ((tegfs make-temporary-filename-local) :select (make-temporary-filename/local))
    )))



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
