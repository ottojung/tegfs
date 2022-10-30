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

%var has-access-for-entry?
%var has-access-for-entry-target?
%var has-access-for-entry-details?

%use (assoc-or) "./euphrates/assoc-or.scm"
%use (hashmap-ref) "./euphrates/ihashmap.scm"
%use (hashset-ref) "./euphrates/ihashset.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (entry-for-local-file?) "./entry-for-local-file-huh.scm"
%use (entry-parent-directory-vid-key) "./entry-parent-directory-vid-key.scm"
%use (filemap-ref-by-vid get-current-filemap/2) "./filemap.scm"
%use (permission-admin? permission-detailsaccess? permission-filemap permission-idset) "./permission.scm"
%use (sharedinfo-sourcepath) "./web-sharedinfo.scm"

(define (has-access-for-entry? perm entry)
  (and perm
       (or (permission-admin? perm)
           (if (entry-for-local-file? entry)
               (let* ((parent-vid (or (assoc-or entry-parent-directory-vid-key entry #f)
                                      (raisu 'entry-does-not-have-parent-vid entry)))
                      (filemap/2 (get-current-filemap/2))
                      (info (filemap-ref-by-vid filemap/2 parent-vid #f))
                      (target-fullpath (and info (sharedinfo-sourcepath info)))
                      (perm-filemap (permission-filemap perm)))
                 (and target-fullpath
                      (not (not (hashmap-ref perm-filemap target-fullpath #f)))))
               (let ((id (cdr (assoc 'id entry)))
                     (idset (permission-idset perm)))
                 (hashset-ref idset id))))))

(define (has-access-for-entry-target? perm entry)
  (has-access-for-entry? perm entry))

(define (has-access-for-entry-details? perm entry)
  (and perm
       (and (permission-detailsaccess? perm)
            (has-access-for-entry? perm entry))))
