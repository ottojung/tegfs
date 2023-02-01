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
  (define-module (tegfs make-permission-bang)
    :export (make-permission!)
    :use-module ((euphrates hashmap) :select (hashmap-set!))
    :use-module ((tegfs make-permission) :select (make-permission))
    :use-module ((tegfs password-to-tokenlike) :select (password->tokenlike))
    :use-module ((tegfs permission) :select (permission-token))
    :use-module ((tegfs webcore-context) :select (context-tempentries))
    )))



(define (make-permission! ctx expiery-time admin? maybepassword dynamic)
  (define tempentries (context-tempentries ctx))
  (define perm
    (make-permission
     expiery-time admin?
     maybepassword dynamic))
  (define token (permission-token perm))
  (hashmap-set! tempentries token perm)
  (when maybepassword
    (let ((tokenlike (password->tokenlike maybepassword)))
      (hashmap-set! tempentries tokenlike perm)))
  perm)
