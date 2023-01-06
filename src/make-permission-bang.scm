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

%var make-permission!

%use (hashmap-set!) "./euphrates/hashmap.scm"
%use (make-permission) "./make-permission.scm"
%use (password->tokenlike) "./password-to-tokenlike.scm"
%use (permission-token) "./permission.scm"
%use (context-tokens) "./web-context.scm"

(define (make-permission! ctx expiery-time admin? maybepassword uploadaccess? detailsaccess? share-longer-than-view?)
  (define tokens (context-tokens ctx))
  (define perm
    (make-permission
     expiery-time admin?
     maybepassword uploadaccess? detailsaccess?
     share-longer-than-view?))
  (define token (permission-token perm))
  (hashmap-set! tokens token perm)
  (when maybepassword
    (let ((tokenlike (password->tokenlike maybepassword)))
      (hashmap-set! tokens tokenlike perm)))
  perm)
