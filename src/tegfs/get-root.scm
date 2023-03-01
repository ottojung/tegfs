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
  (define-module (tegfs get-root)
    :export (get-root get-root/default)
    :use-module ((euphrates absolute-posix-path-q) :select (absolute-posix-path?))
    :use-module ((euphrates append-posix-path) :select (append-posix-path))
    :use-module ((euphrates file-or-directory-exists-q) :select (file-or-directory-exists?))
    :use-module ((euphrates get-current-directory) :select (get-current-directory))
    :use-module ((euphrates make-directories) :select (make-directories))
    :use-module ((euphrates memconst) :select (memconst))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((euphrates system-environment) :select (system-environment-get))
    :use-module ((tegfs root-p) :select (root/p))
    )))



(define ROOT_VAR_NAME "TEGFS_ROOT")
(define root-default
  (memconst
   (system-environment-get ROOT_VAR_NAME)))

(define DEFAULT_ROOT_VAR_NAME "DEFAULT_TEGFS_ROOT")
(define root-default-default
  (memconst
   (system-environment-get DEFAULT_ROOT_VAR_NAME)))

(define get-root/default
  (let ((root-made? #f))
    (lambda _
      (define root0
        (or (root/p)
            (root-default)
            (root-default-default)))
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
