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
  (define-module (tegfs core-categorization)
    :export (core::categorization)
    :use-module ((euphrates append-posix-path) :select (append-posix-path))
    :use-module ((euphrates catch-any) :select (catch-any))
    :use-module ((euphrates profun-accept) :select (profun-set))
    :use-module ((euphrates profun-error) :select (make-profun-error))
    :use-module ((euphrates profun-op-lambda) :select (profun-op-lambda))
    :use-module ((euphrates profun-value) :select (profun-bound-value?))
    :use-module ((euphrates read-string-file) :select (read-string-file))
    :use-module ((tegfs categorization-filename) :select (categorization-filename))
    :use-module ((tegfs get-root) :select (get-root)))))



(define core::categorization
  (profun-op-lambda
   (ctx (categorization-text) (categorization-text-name))

   (cond
    ((profun-bound-value? categorization-text)
     (make-profun-error
      'type-error
      "Variable categorization-text is a return value, it should not be bound"
      categorization-text-name))

    (else
     (let ()
       (define categorization-file
         (append-posix-path (get-root) categorization-filename))
       (define content
         (catch-any
          (lambda _ (read-string-file categorization-file))
          (lambda _ "")))

       (profun-set (categorization-text-name <- content)))))))
