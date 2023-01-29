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

%var core::set-categorization

%use (append-posix-path) "./euphrates/append-posix-path.scm"
%use (profun-accept) "./euphrates/profun-accept.scm"
%use (make-profun-error) "./euphrates/profun-error.scm"
%use (profun-op-lambda) "./euphrates/profun-op-lambda.scm"
%use (profun-unbound-value?) "./euphrates/profun-value.scm"
%use (write-string-file) "./euphrates/write-string-file.scm"
%use (categorization-filename) "./categorization-filename.scm"
%use (get-root) "./get-root.scm"

(define core::set-categorization
  (profun-op-lambda
   (ctx (categorization-text) (categorization-text-name))

   (cond
    ((profun-unbound-value? categorization-text)
     (make-profun-error
      'type-error
      "Variable categorization-text is required to be bound"
      categorization-text-name))

    (else
     (let ()
       (define categorization-file
         (append-posix-path (get-root) categorization-filename))
       (write-string-file categorization-file categorization-text)
       (profun-accept))))))
