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

%run guile

%var permission-constructor
%var permission?
%var permission-token
%var permission-start
%var permission-time
%var permission-admin?
%var permission-uploadaccess?
%var permission-detailsaccess?
%var permission-share-longer-than-view?
%var permission-filemap
%var permission-idset

%use (define-type9) "./euphrates/define-type9.scm"

(define-type9 <permission>
  (permission-constructor token start time admin? uploadaccess? detailsaccess? share-longer-than-view? filemap idset) permission?
  (token permission-token) ;; token string
  (start permission-start) ;; timestamp for when this token was created
  (time permission-time) ;; duration in secods for how long this token is valid
  (admin? permission-admin?) ;; true if user is an admin
  (uploadaccess? permission-uploadaccess?) ;; true if user can create new entries
  (detailsaccess? permission-detailsaccess?) ;; true if user has access to objects details
  (share-longer-than-view? permission-share-longer-than-view?) ;; true if user has access to objects details
  (filemap permission-filemap) ;; hashmap with `keys: target-fullpath that was shared with this permission` and `values: sharedinfos`
  (idset permission-idset) ;; hashset with `values: id of entry that is shared with this permission`
  )
