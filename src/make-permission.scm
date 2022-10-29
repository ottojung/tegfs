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

%var make-permission

%use (make-hashmap) "./euphrates/ihashmap.scm"
%use (make-hashset) "./euphrates/ihashset.scm"
%use (time-get-current-unixtime) "./euphrates/time-get-current-unixtime.scm"
%use (permission-constructor) "./web-permission.scm"

(define (make-permission expiery-time admin? detailsaccess? share-longer-than-view?)
  (define token (generate-token))
  (define start (time-get-current-unixtime))
  (define time expiery-time)
  (define perm
    (permission-constructor
     token start time
     admin? detailsaccess? share-longer-than-view?
     (make-hashmap) (make-hashset)))
  perm)