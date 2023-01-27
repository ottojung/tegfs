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

%run guile

%use (assoc-set-value) "./euphrates/assoc-set-value.scm"
%use (entries-map!) "./tegfs/entries-map-bang.scm"
%use (entry-target-fullpath) "./tegfs/entry-target-fullpath.scm"
%use (get-file-mimetype) "./tegfs/get-file-mimetype.scm"
%use (keyword-entry-registry-path) "./tegfs/keyword-entry-registry-path.scm"
%use (keyword-mimetype) "./tegfs/keyword-mimetype.scm"

(entries-map!
 (lambda (registry-path entry)
   (define fullpath
     (entry-target-fullpath
      (cons (cons keyword-entry-registry-path registry-path) entry)))
   (if (not fullpath) entry
       (let ((new-mimetype (get-file-mimetype fullpath)))
         (assoc-set-value keyword-mimetype new-mimetype entry)))))
