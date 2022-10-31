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
%use (filemap-ref-by-vid) "./filemap.scm"
%use (keyword-entry-parent-directory-vid) "./keyword-entry-parent-directory-vid.scm"
%use (keyword-id) "./keyword-id.scm"
%use (permission-admin? permission-detailsaccess? permission-filemap permission-idset) "./permission.scm"
%use (sharedinfo-sourcepath) "./sharedinfo.scm"

(define (has-access-for-entry? filemap/2 perm entry)
  (and perm
       (or (permission-admin? perm)
           (if (entry-for-local-file? entry)
               (let* ((parent-vid (or (assoc-or keyword-entry-parent-directory-vid entry #f)
                                      (raisu 'entry-does-not-have-parent-vid entry)))
                      (info (filemap-ref-by-vid filemap/2 parent-vid #f))
                      (target-fullpath (and info (sharedinfo-sourcepath info)))
                      (perm-filemap (permission-filemap perm)))
                 (and target-fullpath
                      (not (not (hashmap-ref perm-filemap target-fullpath #f)))))
               (let ((id (cdr (assoc keyword-id entry)))
                     (idset (permission-idset perm)))
                 (hashset-ref idset id))))))

(define (has-access-for-entry-target? filemap/2 perm entry)
  (has-access-for-entry? filemap/2 perm entry))

(define (has-access-for-entry-details? filemap/2 perm entry)
  (and perm
       (and (permission-detailsaccess? perm)
            (has-access-for-entry? filemap/2 perm entry))))
