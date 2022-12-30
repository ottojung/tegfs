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

%var tegfs-login-by-key

%use (hashmap-delete! hashmap-ref) "./euphrates/hashmap.scm"
%use (permission-still-valid?) "./permission-still-valid-huh.scm"
%use (context-tokens) "./web-context.scm"

(define (tegfs-login-by-key ctx key)
  (define tokens (context-tokens ctx))
  (define existing (hashmap-ref tokens key #f))
  (define perm
    (and existing
         (if (permission-still-valid? existing)
             existing
             (begin
               (hashmap-delete! tokens key)
               #f))))

  perm)

