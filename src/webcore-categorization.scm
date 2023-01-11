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

%run guile

%var webcore::categorization

%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (catch-any) "./euphrates/catch-any.scm"
%use (profun-set) "./euphrates/profun-accept.scm"
%use (make-profun-error) "./euphrates/profun-error.scm"
%use (profun-op-lambda) "./euphrates/profun-op-lambda.scm"
%use (profun-bound-value?) "./euphrates/profun-value.scm"
%use (read-string-file) "./euphrates/read-string-file.scm"
%use (categorization-filename) "./categorization-filename.scm"
%use (get-root) "./get-root.scm"
%use (can-view-categorization?) "./webcore-access.scm"
%use (webcore::permissions/p) "./webcore-parameters.scm"

(define (webcore::categorization web::context)
  (profun-op-lambda
   :with-env
   (ctx (categorization-text) (categorization-text-name))

   (define perm (webcore::permissions/p))

   (cond
    ((profun-bound-value? categorization-text)
     (make-profun-error
      'type-error
      "Variable categorization-text is a return value, it should not be bound"
      categorization-text-name))

    ((not (can-view-categorization? perm))
     (make-profun-error
      'permission-denied
      "This user cannot see tags categorization"))

    (else
     (let ()
       (define categorization-file
         (append-posix-path (get-root) categorization-filename))
       (define content
         (catch-any
          (lambda _ (read-string-file categorization-file))
          (lambda _ "")))

       (profun-set (categorization-text-name <- content)))))))
