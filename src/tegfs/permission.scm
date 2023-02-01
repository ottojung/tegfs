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
  (define-module (tegfs permission)
    :export (permission-constructor permission? permission-token permission-date permission-stime permission-admin? permission-maybepassword permission-uploadaccess? permission-entry-view-access? permission-entry-modify-access? permission-cat-view-access? permission-cat-modify-access? permission-share-longer-than-view? permission-filemap permission-idset)
    :use-module ((euphrates assq-or) :select (assq-or))
    :use-module ((euphrates define-type9) :select (define-type9))
    )))



(define-type9 <permission>
  (permission-constructor token date stime admin? dynamic filemap idset) permission?
  (token permission-token) ;; token string
  (date permission-date) ;; timestamp for when this token was created
  (stime permission-stime) ;; duration in secods for how long this token is valid
  (admin? permission-admin?) ;; true for superuser users
  (dynamic permission-dynamic) ;; alist of particular permissions, i.e. if can upload new entries
  (filemap permission-filemap) ;; hashmap with `keys: target-fullpath that was shared with this permission` and `values: sharedinfos`
  (idset permission-idset) ;; hashset with `values: id of entry that is shared with this permission`
  )

(define (permission-maybepassword perm) ;; *hashed* password, or #f.
  (assq-or 'maybepassword (permission-dynamic perm)))

(define (permission-uploadaccess? perm) ;; true if user can create new entries
  (assq-or 'uploadaccess? (permission-dynamic perm)))

(define (permission-entry-view-access? perm) ;; true if user has access to objects details
  (assq-or 'entry-view-access? (permission-dynamic perm)))

(define (permission-entry-modify-access? perm) ;; true if user can modify (AND DELETE) existing entries
  (assq-or 'entry-modify-access? (permission-dynamic perm)))

(define (permission-cat-view-access? perm) ;; true if user has read access to tags categorization
  (assq-or 'cat-view-access? (permission-dynamic perm)))

(define (permission-cat-modify-access? perm) ;; true if user has write access to tags categorization
  (assq-or 'cat-modify-access? (permission-dynamic perm)))

(define (permission-share-longer-than-view? perm) ;; true if user can share entries for longer than viewing allows. useful for admins
  (assq-or 'sharelonger-than-view? (permission-dynamic perm)))
