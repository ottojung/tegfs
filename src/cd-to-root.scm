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

%var cd-to-root

%use (root/p) "./root-p.scm"

(define (cd-to-root)
  (define root (root/p))

  (unless root
    (fatal "Cannot cd into root because $~a env variable is not defined" ROOT_VAR_NAME))

  (unless (file-or-directory-exists? root)
    (make-directories root))

  (chdir root))
