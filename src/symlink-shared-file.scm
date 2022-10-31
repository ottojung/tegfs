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

%var symlink-shared-file

%use (absolute-posix-path?) "./euphrates/absolute-posix-path-q.scm"
%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (catch-any) "./euphrates/catch-any.scm"
%use (file-or-directory-exists?) "./euphrates/file-or-directory-exists-q.scm"
%use (get-current-directory) "./euphrates/get-current-directory.scm"
%use (web-context/p) "./web-context-p.scm"
%use (context-sharedir) "./web-context.scm"

(define (symlink-shared-file target-fullpath sharedname)
  (define ctx (web-context/p))
  (define sharedir (context-sharedir ctx))
  (define shared-fullpath (append-posix-path sharedir sharedname))
  (define target-fullpath/abs
    (if (absolute-posix-path? target-fullpath) target-fullpath
        (append-posix-path (get-current-directory) target-fullpath)))

  (unless (file-or-directory-exists? shared-fullpath)
    (catch-any
     (lambda _
       (symlink target-fullpath/abs shared-fullpath))
     (lambda errors
       (display "Error symlinking: ")
       (write errors)
       (newline)))))
