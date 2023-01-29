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

;; Replaces `original-entry' by the `updated-entry'.
;; Note that if `updated-entry' is `#f', then the `original-entry' is deleted.
;; If `keyword-target` of the entry is updated or deleted, then the accompaying file is also renamed or deleted.
%var core::update-entry

%use (catchu-case) "./euphrates/catchu-case.scm"
%use (profun-accept) "./euphrates/profun-accept.scm"
%use (make-profun-error) "./euphrates/profun-error.scm"
%use (profun-op-lambda) "./euphrates/profun-op-lambda.scm"
%use (profun-request-value) "./euphrates/profun-request-value.scm"
%use (profun-unbound-value?) "./euphrates/profun-value.scm"
%use (update-entry) "./update-entry.scm"

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
