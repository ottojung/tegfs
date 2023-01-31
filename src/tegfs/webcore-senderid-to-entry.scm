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
  (define-module (tegfs webcore-senderid-to-entry)
    :export (webcore::senderid->entry)
    :use-module ((euphrates profun-accept) :select (profun-set profun-set-meta))
    :use-module ((euphrates profun-error) :select (make-profun-error))
    :use-module ((euphrates profun-op-lambda) :select (profun-op-lambda))
    :use-module ((euphrates profun-reject) :select (profun-reject))
    :use-module ((euphrates profun-value) :select (profun-bound-value? profun-unbound-value?))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((tegfs entry-limit-fields) :select (entry-limit-fields))
    :use-module ((tegfs filemap) :select (filemap-ref-by-senderid))
    :use-module ((tegfs permission) :select (permission?))
    :use-module ((tegfs sharedinfo) :select (sharedinfo-entry))
    :use-module ((tegfs web-context) :select (context-filemap/2))
    :use-module ((tegfs webcore-parameters) :select (webcore::permissions/p))
    )))



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
