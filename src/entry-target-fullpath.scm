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

%var entry-target-fullpath

%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (path-normalize) "./euphrates/path-normalize.scm"

%use (get-root) "./get-root.scm"
%use (entry-registry-path-key) "./entry-registry-path-key.scm"
%use (entry-parent-directory-key) "./entry-parent-directory-key.scm"
%use (a-weblink?) "./a-weblink-q.scm"

(define (entry-target-fullpath entry)
  (define target-p (assoc 'target entry))
  (and target-p
       (let ((target (cdr target-p)))
         (if (a-weblink? target) target
             (path-normalize
              (let* ((parent-directory-p (assoc entry-parent-directory-key entry))
                     (parent-directory (and parent-directory-p (cdr parent-directory-p)))
                     (registry-p (assoc entry-registry-path-key entry))
                     (registry-dir (and registry-p (dirname (cdr registry-p))))
                     (directory (or parent-directory registry-dir)))
                (unless directory
                  (raisu 'entry-does-no-have-parent-directory-info
                         entry-parent-directory-key
                         entry-registry-path-key))
                (append-posix-path (get-root) directory target)))))))
