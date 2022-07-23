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
%var get-root/default

%use (raisu) "./euphrates/raisu.scm"
%use (absolute-posix-path?) "./euphrates/absolute-posix-path-q.scm"
%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (get-current-directory) "./euphrates/get-current-directory.scm"
%use (system-environment-get) "./euphrates/system-environment.scm"
%use (file-or-directory-exists?) "./euphrates/file-or-directory-exists-q.scm"
%use (make-directories) "./euphrates/make-directories.scm"
%use (memconst) "./euphrates/memconst.scm"

%use (root/p) "./root-p.scm"

(define ROOT_VAR_NAME "TEGFS_ROOT")
(define root-default
  (memconst
   (system-environment-get ROOT_VAR_NAME)))

(define get-root/default
  (let ((root-made? #f))
    (lambda _
      (define root0
        (or (root/p)
            (root-default)))
      (define root
        (and root0
             (if (absolute-posix-path? root0) root0
                 (append-posix-path (get-current-directory) root0))))

      (when root
        (unless root-made?
          (set! root-made? #t)
          (unless (file-or-directory-exists? root)
            (make-directories root))))

      root)))

(define (get-root)
  (or (get-root/default)
      (raisu 'unknown-root
             "Root is unknown because $~a env variable is not defined" ROOT_VAR_NAME)))
