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

%var permission-constructor
%var permission?
%var permission-token
%var permission-start
%var permission-time
%var permission-admin?
%var permission-detailsaccess?
%var permission-filemap
%var permission-idset

%use (define-type9) "./euphrates/define-type9.scm"

(define-type9 <permission>
  (permission-constructor token start time admin? detailsaccess? filemap idset) permission?
  (token permission-token) ;; token string
  (start permission-start) ;; timestamp for when this token was created
  (time permission-time) ;; duration in secods for how long this token is valid
  (admin? permission-admin?) ;; true if user is an admin
  (detailsaccess? permission-detailsaccess?) ;; true if user has access to objects details
  (filemap permission-filemap) ;; hashmap with `keys: target-fullpath that was shared with this permission` and `values: sharedinfos`
  (idset permission-idset) ;; hashset with `values: id of entry that is shared with this permission`
  )
