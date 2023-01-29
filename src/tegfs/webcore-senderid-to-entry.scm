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

%var webcore::senderid->entry

%use (profun-set profun-set-meta) "./euphrates/profun-accept.scm"
%use (make-profun-error) "./euphrates/profun-error.scm"
%use (profun-op-lambda) "./euphrates/profun-op-lambda.scm"
%use (profun-reject) "./euphrates/profun-reject.scm"
%use (profun-bound-value? profun-unbound-value?) "./euphrates/profun-value.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (entry-limit-fields) "./entry-limit-fields.scm"
%use (filemap-ref-by-senderid) "./filemap.scm"
%use (permission?) "./permission.scm"
%use (sharedinfo-entry) "./sharedinfo.scm"
%use (context-filemap/2) "./web-context.scm"
%use (webcore::permissions/p) "./webcore-parameters.scm"

(define webcore::senderid->entry
  (lambda (web::context)
    (define filemap/2 (context-filemap/2 web::context))

    (profun-op-lambda
     :with-env
     (ctx (senderid entry/out) (senderid-name entry-name))

     (define info (filemap-ref-by-senderid filemap/2 senderid #f))
     (define perm (webcore::permissions/p))

     (cond
      ((not (permission? perm))
       (make-profun-error 'permission-denied "Not authorized. Missing key?"))

      ((profun-unbound-value? senderid)
       (make-profun-error 'type-error "Senderid must be given"))

      ((profun-bound-value? entry/out)
       (make-profun-error 'type-error "Entry variable must be free"))

      ((not info)
       (profun-reject))

      (info
       (let ()
         (define entry0 (sharedinfo-entry info))
         (define entry (entry-limit-fields filemap/2 perm entry0))
         (if (null? entry)
             (profun-reject)
             (profun-set-meta
              (entry-name <- entry0)
              (profun-set (entry-name <- entry))))))

      (else
       (raisu 'impossible-case))))))
