;;;; Copyright (C) 2022, 2023  Otto Jung
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

%run guile

%var has-access-for-entry?
%var has-access-for-entry-target?
%var has-access-for-entry-details?
%var can-modify-entry?
%var can-upload?
%var can-view-categorization?
%var can-modify-categorization?

%use (assoc-or) "./euphrates/assoc-or.scm"
%use (hashmap-ref) "./euphrates/hashmap.scm"
%use (hashset-has?) "./euphrates/hashset.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (entry-for-local-file?) "./entry-for-local-file-huh.scm"
%use (filemap-ref-by-senderid) "./filemap.scm"
%use (keyword-entry-parent-directory-senderid) "./keyword-entry-parent-directory-senderid.scm"
%use (keyword-id) "./keyword-id.scm"
%use (permission-admin? permission-cat-modify-access? permission-cat-view-access? permission-entry-modify-access? permission-entry-view-access? permission-filemap permission-idset permission-uploadaccess?) "./permission.scm"
%use (sharedinfo-sourcepath) "./sharedinfo.scm"

(define (has-access-for-entry? filemap/2 perm entry)
  (and perm
       (or (permission-admin? perm)
           (if (entry-for-local-file? entry)
               (let* ((parent-senderid (or (assoc-or keyword-entry-parent-directory-senderid entry #f)
                                      (raisu 'entry-does-not-have-parent-senderid entry)))
                      (info (filemap-ref-by-senderid filemap/2 parent-senderid #f))
                      (target-fullpath (and info (sharedinfo-sourcepath info)))
                      (perm-filemap (permission-filemap perm)))
                 (and target-fullpath
                      (not (not (hashmap-ref perm-filemap target-fullpath #f)))))
               (let ((id (cdr (assoc keyword-id entry)))
                     (idset (permission-idset perm)))
                 (hashset-has? idset id))))))

(define (has-access-for-entry-target? filemap/2 perm entry)
  (has-access-for-entry? filemap/2 perm entry))

(define (has-access-for-entry-details? filemap/2 perm entry)
  (and perm
       (or (permission-admin? perm)
           (and (permission-entry-view-access? perm)
                (has-access-for-entry? filemap/2 perm entry)))))

(define (can-modify-entry? filemap/2 perm entry)
  (and perm
       (or (permission-admin? perm)
           (and (permission-entry-modify-access? perm)
                (has-access-for-entry? filemap/2 perm entry)))))

(define (can-upload? perm)
  (and perm
       (or (permission-admin? perm)
           (permission-uploadaccess? perm))))

(define (can-view-categorization? perm)
  (and perm
       (or (permission-admin? perm)
           (permission-cat-view-access? perm))))

(define (can-modify-categorization? perm)
  (and perm
       (or (permission-admin? perm)
           (permission-cat-modify-access? perm))))
