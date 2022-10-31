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

%var make-permission!

%use (hashmap-set!) "./euphrates/ihashmap.scm"
%use (make-permission) "./make-permission.scm"
%use (permission-token) "./permission.scm"
%use (web-context/p) "./web-context-p.scm"
%use (context-tokens) "./web-context.scm"

(define (make-permission! expiery-time admin? detailsaccess? share-longer-than-view?)
  (define ctx (web-context/p))
  (define tokens (context-tokens ctx))
  (define perm
    (make-permission
     expiery-time admin?
     detailsaccess?
     share-longer-than-view?))
  (define token (permission-token perm))
  (hashmap-set! tokens token perm)
  perm)
