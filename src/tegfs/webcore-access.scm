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

(cond-expand
 (guile
  (define-module (tegfs webcore-access)
    :export (has-access-for-entry? has-access-for-entry-target? has-access-for-entry-details? can-modify-entry? can-upload? can-view-categorization? can-modify-categorization? can-share-longer-than-view? can-manage-tempentries?)
    :use-module ((euphrates assoc-or) :select (assoc-or))
    :use-module ((euphrates hashmap) :select (hashmap-ref))
    :use-module ((euphrates hashset) :select (hashset-has?))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((tegfs entry-for-local-file-huh) :select (entry-for-local-file?))
    :use-module ((tegfs filemap) :select (filemap-ref-by-senderid))
    :use-module ((tegfs keyword-entry-parent-directory-senderid) :select (keyword-entry-parent-directory-senderid))
    :use-module ((tegfs keyword-id) :select (keyword-id))
    :use-module ((tegfs permission) :select (permission-admin? permission-cat-modify-access? permission-cat-view-access? permission-entry-modify-access? permission-entry-view-access? permission-filemap permission-idset permission-share-longer-than-view? permission-uploadaccess?))
    :use-module ((tegfs sharedinfo) :select (sharedinfo-sourcepath))
    )))



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

(define (can-share-longer-than-view? perm) ;; true if user can share entries for longer than viewing allows. useful for admins
  (and perm
       (or (permission-admin? perm)
           (permission-share-longer-than-view? perm))))

(define (can-manage-tempentries? perm)
  ;; True if user can create and edit temporary entries.
  ;; Generally, only admins should be allowed to do this because
  ;;  1) temporary entries can contain security critical information,
  ;;  2) temporary entries can be spammed with.

  (and perm (permission-admin? perm)))
