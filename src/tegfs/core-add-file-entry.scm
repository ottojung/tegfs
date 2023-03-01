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
  (define-module (tegfs core-add-file-entry)
    :export (core::add-file-entry)
    :use-module ((euphrates profun-accept) :select (profun-accept))
    :use-module ((euphrates profun-op-lambda) :select (profun-op-lambda))
    :use-module ((euphrates profun-request-value) :select (profun-request-value))
    :use-module ((euphrates profun-value) :select (profun-unbound-value?))
    :use-module ((tegfs add-file-entry) :select (add-file-entry))
    )))



(define core::add-file-entry
  (profun-op-lambda
   (ctx (full-filepath entry) (F-name E-name))

   (cond
    ((profun-unbound-value? entry)
     (profun-request-value E-name))
    ((profun-unbound-value? full-filepath)
     (profun-request-value F-name))

    (else
     (add-file-entry full-filepath entry)
     (profun-accept)))))
