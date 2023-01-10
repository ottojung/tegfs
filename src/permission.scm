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

%var permission-constructor
%var permission?
%var permission-token
%var permission-start
%var permission-time
%var permission-admin?
%var permission-maybepassword
%var permission-uploadaccess?
%var permission-detailsaccess?
%var permission-categorizationaccess?
%var permission-share-longer-than-view?
%var permission-filemap
%var permission-idset

%use (assq-or) "./euphrates/assq-or.scm"
%use (define-type9) "./euphrates/define-type9.scm"

(define-type9 <permission>
  (permission-constructor token start time admin? dynamic filemap idset) permission?
  (token permission-token) ;; token string
  (start permission-start) ;; timestamp for when this token was created
  (time permission-time) ;; duration in secods for how long this token is valid
  (admin? permission-admin?) ;; true for superuser users
  (dynamic permission-dynamic) ;; alist of particular permissions, i.e. if can upload new entries
  (filemap permission-filemap) ;; hashmap with `keys: target-fullpath that was shared with this permission` and `values: sharedinfos`
  (idset permission-idset) ;; hashset with `values: id of entry that is shared with this permission`
  )

(define (permission-maybepassword perm) ;; *hashed* password, or #f.
  (assq-or 'maybepassword (permission-dynamic perm)))

(define (permission-uploadaccess? perm) ;; true if user can create new entries
  (assq-or 'uploadaccess? (permission-dynamic perm)))

(define (permission-detailsaccess? perm) ;; true if user has access to objects details
  (assq-or 'detailsaccess? (permission-dynamic perm)))

(define (permission-categorizationaccess? perm) ;; true if user has access to tags categorization
  (assq-or 'categorizationaccess? (permission-dynamic perm)))

(define (permission-share-longer-than-view? perm) ;; true if user can share entries for longer than viewing allows. useful for admins
  (assq-or 'sharelonger-than-view? (permission-dynamic perm)))
