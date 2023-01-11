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

%var entry-target-fullpath

%use (absolute-posix-path?) "./euphrates/absolute-posix-path-q.scm"
%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (assq-or) "./euphrates/assq-or.scm"
%use (path-normalize) "./euphrates/path-normalize.scm"
%use (string-drop-n) "./euphrates/string-drop-n.scm"
%use (a-weblink?) "./a-weblink-q.scm"
%use (get-root) "./get-root.scm"
%use (keyword-entry-parent-directory) "./keyword-entry-parent-directory.scm"
%use (keyword-entry-registry-path) "./keyword-entry-registry-path.scm"
%use (keyword-id) "./keyword-id.scm"
%use (keyword-target) "./keyword-target.scm"

(define (entry-target-fullpath entry)
  (define target-p (assoc keyword-target entry))
  (and target-p
       (let ((target/1 (cdr target-p))
             (id (assq-or keyword-id entry #f)))
         (cond
          ((not (string? target/1)) #f)
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
                     (append-posix-path (get-root) directory target))))))))))
