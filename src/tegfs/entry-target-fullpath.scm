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

(cond-expand
 (guile
  (define-module (tegfs entry-target-fullpath)
    :export (entry-target-fullpath)
    :use-module ((euphrates absolute-posix-path-q) :select (absolute-posix-path?))
    :use-module ((euphrates append-posix-path) :select (append-posix-path))
    :use-module ((euphrates assq-or) :select (assq-or))
    :use-module ((euphrates path-normalize) :select (path-normalize))
    :use-module ((euphrates string-drop-n) :select (string-drop-n))
    :use-module ((tegfs a-weblink-q) :select (a-weblink?))
    :use-module ((tegfs entry-get-target) :select (entry-get-target))
    :use-module ((tegfs get-root) :select (get-root))
    :use-module ((tegfs keyword-entry-parent-directory) :select (keyword-entry-parent-directory))
    :use-module ((tegfs keyword-entry-registry-path) :select (keyword-entry-registry-path))
    :use-module ((tegfs keyword-id) :select (keyword-id)))))



(define (entry-target-fullpath entry)
  (define target/1 (entry-get-target entry))
  (define id (assq-or keyword-id entry #f))
  (cond
   ((not target/1) #f)
   ((a-weblink? target/1) target/1)
   ((absolute-posix-path? id) id)
   (else
    (let* ((target/0 (path-normalize target/1))
           (target (if (absolute-posix-path? target/0) (string-drop-n 1 target/0) target/0)))
      (let* ((parent-directory-p (assq keyword-entry-parent-directory entry))
             (parent-directory (and parent-directory-p (cdr parent-directory-p)))
             (registry-p (assq keyword-entry-registry-path entry))
             (registry-dir (and registry-p (dirname (cdr registry-p))))
             (directory (or parent-directory registry-dir)))
        (and directory
             (path-normalize
              (append-posix-path (get-root) directory target))))))))
