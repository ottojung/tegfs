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
  (define-module (tegfs webcore-update-entry)
    :export (webcore::update-entry)
    :use-module ((euphrates catchu-case) :select (catchu-case))
    :use-module ((euphrates profun-accept) :select (profun-accept))
    :use-module ((euphrates profun-error) :select (make-profun-error))
    :use-module ((euphrates profun-meta-key) :select (profun-meta-key))
    :use-module ((euphrates profun-op-lambda) :select (profun-op-lambda))
    :use-module ((euphrates profun-request-value) :select (profun-request-value))
    :use-module ((euphrates profun-value) :select (profun-unbound-value?))
    :use-module ((tegfs update-entry) :select (update-entry))
    :use-module ((tegfs web-context) :select (context-filemap/2))
    :use-module ((tegfs webcore-access) :select (can-modify-entry?))
    :use-module ((tegfs webcore-parameters) :select (webcore::permissions/p)))))

;; Replaces `original-entry' by the `updated-entry'.
;; Note that if `updated-entry' is `#f', then the `original-entry' is deleted.
;; If `keyword-target` of the entry is updated or deleted, then the accompaying file is also renamed or deleted.


(define (webcore::update-entry webcore::context)
  (define filemap/2 (context-filemap/2 webcore::context))
  (profun-op-lambda
   :with-env env
   (ctx (original-entry/0 updated-entry) (O-name U-name))

   (define perm (webcore::permissions/p))

   (cond
    ((profun-unbound-value? original-entry/0)
     (profun-request-value O-name))
    ((profun-unbound-value? updated-entry)
     (profun-request-value U-name))

    (else
     (let ()
       (define original-entry
         (env (profun-meta-key O-name)))

       (cond
        ((profun-unbound-value? original-entry)
         (make-profun-error 'type-error "Cannot confirm that the entry passed is the actual entry from the database"))
        ((not (can-modify-entry? filemap/2 perm original-entry))
         (make-profun-error 'permission-denied "This user cannot modify this entry"))

        (else
         (catchu-case
          (begin
            (update-entry original-entry updated-entry)
            (profun-accept))

          (('type-error:original-entry-does-not-have-an-id)
           (make-profun-error
            'type-error
            "Entry does not have an id"
            original-entry))))))))))
