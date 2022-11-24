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

%var get-admin-permissions

%use (memconst) "./euphrates/memconst.scm"
%use (make-permission) "./make-permission.scm"

(define get-admin-permissions
  (memconst
   (let ((expiery-time +inf.0)
         (admin? #t)
         (detailsaccess? #t)
         (share-longer-than-view? #t))
     (make-permission
      expiery-time admin?
      detailsaccess?
      share-longer-than-view?))))
