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
  (define-module (tegfs core-update-entry)
    :export (core::update-entry)
    :use-module ((euphrates catchu-case) :select (catchu-case))
    :use-module ((euphrates profun-accept) :select (profun-accept))
    :use-module ((euphrates profun-error) :select (make-profun-error))
    :use-module ((euphrates profun-op-lambda) :select (profun-op-lambda))
    :use-module ((euphrates profun-request-value) :select (profun-request-value))
    :use-module ((euphrates profun-value) :select (profun-unbound-value?))
    :use-module ((tegfs update-entry) :select (update-entry))
    )))

;; Replaces `original-entry' by the `updated-entry'.
;; Note that if `updated-entry' is `#f', then the `original-entry' is deleted.
;; If `keyword-target` of the entry is updated or deleted, then the accompaying file is also renamed or deleted.


(define core::update-entry
  (profun-op-lambda
   (ctx (original-entry updated-entry) (O-name U-name))

   (cond
    ((profun-unbound-value? original-entry)
     (profun-request-value O-name))
    ((profun-unbound-value? updated-entry)
     (profun-request-value U-name))

    (else
     (catchu-case
      (begin
        (update-entry original-entry updated-entry)
        (profun-accept))

      (('type-error:original-entry-does-not-have-an-id)
       (make-profun-error
        'type-error
        "Entry does not have an id"
        original-entry)))))))
