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
  (define-module (tegfs webcore-share-entry-preview)
    :export (webcore::share-entry-preview)
    :use-module ((euphrates file-or-directory-exists-q) :select (file-or-directory-exists?))
    :use-module ((tegfs get-preview-path) :select (get-preview-path))
    :use-module ((tegfs webcore-share-entry-generic) :select (webcore::share-entry-generic)))))



(define webcore::share-entry-preview
  (webcore::share-entry-generic
   (lambda (target-fullpath)
     (define ret (get-preview-path target-fullpath))
     (and ret (file-or-directory-exists? ret) ret))))
