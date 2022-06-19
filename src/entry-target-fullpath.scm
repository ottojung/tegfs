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

%use (root/p) "./root-p.scm"
%use (entry-registry-path-key) "./entry-registry-path-key.scm"

(define (entry-target-fullpath entry)
  (define target-p (assoc 'target entry))
  (and target-p
       (let* ((registry-dir (dirname (cdr (assoc entry-registry-path-key entry)))))
         (append-posix-path (root/p) registry-dir (cdr target-p)))))
