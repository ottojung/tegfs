;;;; Copyright (C) 2023  Otto Jung
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

;; This is an example that recalculates all entries' mimetypes.

(cond-expand
 (guile
  (define-module (example recalculate-mimetypes)
    :use-module ((euphrates assoc-set-value) :select (assoc-set-value))
    :use-module ((tegfs entries-map-bang) :select (entries-map!))
    :use-module ((tegfs entry-target-fullpath) :select (entry-target-fullpath))
    :use-module ((tegfs get-file-mimetype) :select (get-file-mimetype))
    :use-module ((tegfs keyword-entry-registry-path) :select (keyword-entry-registry-path))
    :use-module ((tegfs keyword-mimetype) :select (keyword-mimetype)))))

(entries-map!
 (lambda (registry-path entry)
   (define fullpath
     (entry-target-fullpath
      (cons (cons keyword-entry-registry-path registry-path) entry)))
   (if (not fullpath) entry
       (let ((new-mimetype (get-file-mimetype fullpath)))
         (assoc-set-value keyword-mimetype new-mimetype entry)))))
