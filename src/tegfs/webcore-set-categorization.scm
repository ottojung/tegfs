;;;; Copyright (C) 2023  Otto Jung
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
  (define-module (tegfs webcore-set-categorization)
    :export (webcore::set-categorization)
    :use-module ((euphrates append-posix-path) :select (append-posix-path))
    :use-module ((euphrates profun-accept) :select (profun-accept))
    :use-module ((euphrates profun-error) :select (make-profun-error))
    :use-module ((euphrates profun-op-lambda) :select (profun-op-lambda))
    :use-module ((euphrates profun-value) :select (profun-unbound-value?))
    :use-module ((euphrates write-string-file) :select (write-string-file))
    :use-module ((tegfs categorization-filename) :select (categorization-filename))
    :use-module ((tegfs get-root) :select (get-root))
    :use-module ((tegfs webcore-access) :select (can-modify-categorization?))
    :use-module ((tegfs webcore-get-current-permissions) :select (webcore::get-current-permissions))
    )))



(define (webcore::set-categorization web::context)
  (profun-op-lambda
   :with-env
   (ctx (categorization-text) (categorization-text-name))

   (define perm (webcore::get-current-permissions))

   (cond
    ((profun-unbound-value? categorization-text)
     (make-profun-error
      'type-error
      "Variable categorization-text is required to be bound"
      categorization-text-name))

    ((not (can-modify-categorization? perm))
     (make-profun-error
      'permission-denied
      "This user cannot modify tags categorization"))

    (else
     (let ()
       (define categorization-file
         (append-posix-path (get-root) categorization-filename))
       (write-string-file categorization-file categorization-text)
       (profun-accept))))))
