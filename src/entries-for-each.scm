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
%use (open-file-port) "./euphrates/open-file-port.scm"
%use (read-list) "./euphrates/read-list.scm"

%use (root/p) "./root-p.scm"
%use (get-registry-files) "./get-registry-files.scm"
%use (entry-registry-path-key) "./entry-registry-path-key.scm"

(define (entries-for-each fn)
  (for-each
   (lambda (registry-path0)
     (define registry-path (append-posix-path registry-path0))
     (define registry-fullpath (append-posix-path (root/p) registry-path))
     (define input-port (open-file-port registry-fullpath "r"))
     (for-each (lambda (x)
                 (fn (cons (cons entry-registry-path-key registry-path) x)))
               (read-list input-port)))
   (get-registry-files)))
