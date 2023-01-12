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
%var webcore::update-entry

%use (assq-or) "./euphrates/assq-or.scm"
%use (file-delete) "./euphrates/file-delete.scm"
%use (profun-accept) "./euphrates/profun-accept.scm"
%use (make-profun-error) "./euphrates/profun-error.scm"
%use (profun-meta-key) "./euphrates/profun-meta-key.scm"
%use (profun-op-lambda) "./euphrates/profun-op-lambda.scm"
%use (profun-request-value) "./euphrates/profun-request-value.scm"
%use (profun-unbound-value?) "./euphrates/profun-value.scm"
%use (entries-map!) "./entries-map-bang.scm"
%use (entry-target-fullpath) "./entry-target-fullpath.scm"
%use (keyword-id) "./keyword-id.scm"
%use (context-filemap/2) "./web-context.scm"
%use (can-modify-entry?) "./webcore-access.scm"

(define (webcore::update-entry webcore::context)
  (define filemap/2 (context-filemap/2 webcore::context))
  (profun-op-lambda
   :with-env env
   (ctx (original-entry/0 updated-entry) (O-name U-name))

   (define (continue id)
     (define updated-target-fullpath
       (entry-target-fullpath updated-entry))

     (define (update iterated-entry)
       (define original-target
         (entry-target-fullpath iterated-entry))
       (unless (equal? original-target updated-target)
         (if (and updated-target
                  (not (string-empty? updated-target)))
             (rename-file original-target updated-target)
             (file-delete original-target)))
       updated-entry)

     (define (mapper iterated-entry)
       (define iterated-id
         (assq-or keyword-id original-entry #f))
       (if (equal? id iterated-id)
           (update iterated-entry)
           iterated-entry))

     (entries-map! mapper)
     (profun-accept))

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
        ((not (can-modify-entry? filemap/2 perm entry))
         (make-profun-error 'permission-denied "This user cannot modify this entry"))

        (else
         (let ()
           (define id
             (assq-or keyword-id original-entry #f))
           (if id
               (continue id)
               (make-profun-error
                'type-error
                "Entry does not have an id"
                original-entry))))))))))
