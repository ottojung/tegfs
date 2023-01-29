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

(cond-expand
 (guile
  (define-module (tegfs tegfs-login-by-key)
    :export (tegfs-login-by-key)
    :use-module ((euphrates hashmap) :select (hashmap-delete! hashmap-ref))
    :use-module ((tegfs permission-still-valid-huh) :select (permission-still-valid?))
    :use-module ((tegfs web-context) :select (context-tokens)))))



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

