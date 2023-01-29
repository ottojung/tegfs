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
  (define-module (tegfs symlink-shared-file)
    :export (symlink-shared-file)
    :use-module ((euphrates absolute-posix-path-q) :select (absolute-posix-path?))
    :use-module ((euphrates append-posix-path) :select (append-posix-path))
    :use-module ((euphrates catch-any) :select (catch-any))
    :use-module ((euphrates file-or-directory-exists-q) :select (file-or-directory-exists?))
    :use-module ((euphrates get-current-directory) :select (get-current-directory))
    :use-module ((tegfs web-context) :select (context-sharedir))
    :use-module ((tegfs web-get-shared-fullpath) :select (web::get-shared-fullpath)))))



(define (symlink-shared-file ctx target-fullpath recepientid)
  (define sharedir (context-sharedir ctx))
  (define shared-fullpath
    (web::get-shared-fullpath
     sharedir target-fullpath recepientid))
  (define target-fullpath/abs
    (if (absolute-posix-path? target-fullpath) target-fullpath
        (append-posix-path (get-current-directory) target-fullpath)))

  (unless (file-or-directory-exists? shared-fullpath)
    (catch-any
     (lambda _
       (symlink target-fullpath/abs shared-fullpath))
     (lambda errors
       (parameterize ((current-output-port (current-error-port)))
         (display "Error symlinking: ")
         (write errors)
         (newline))))))
