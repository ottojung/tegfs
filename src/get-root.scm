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

%var get-root
%var ROOT_VAR_NAME

%use (raisu) "./euphrates/raisu.scm"
%use (absolute-posix-path?) "./euphrates/absolute-posix-path-q.scm"
%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (get-current-directory) "./euphrates/get-current-directory.scm"

%use (root/p) "./root-p.scm"

(define ROOT_VAR_NAME "TEGFS_ROOT")

(define (get-root)
  (define root0
    (or (root/p)
        (raisu 'unknown-root
               "Root is unknown because $~a env variable is not defined" ROOT_VAR_NAME)))
  (define root
    (if (absolute-posix-path? root0) root0
        (append-posix-path (get-current-directory) root0)))

  root)

