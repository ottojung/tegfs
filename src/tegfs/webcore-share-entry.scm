;;;; Copyright (C) 2022, 2023  Otto Jung
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
  (define-module (tegfs webcore-share-entry)
    :export (webcore::share-entry)
    :use-module ((euphrates assq-or) :select (assq-or))
    :use-module ((euphrates hashset) :select (hashset-add!))
    :use-module ((euphrates profun-accept) :select (profun-accept))
    :use-module ((euphrates profun-error) :select (make-profun-error))
    :use-module ((euphrates profun-meta-key) :select (profun-meta-key))
    :use-module ((euphrates profun-op-lambda) :select (profun-op-lambda))
    :use-module ((euphrates profun-value) :select (profun-bound-value?))
    :use-module ((euphrates raisu) :select (raisu))
    :use-module ((tegfs filemap) :select (filemap-ref-by-senderid))
    :use-module ((tegfs keyword-entry-parent-directory-senderid) :select (keyword-entry-parent-directory-senderid))
    :use-module ((tegfs keyword-id) :select (keyword-id))
    :use-module ((tegfs permission) :select (permission-idset))
    :use-module ((tegfs sharedinfo) :select (sharedinfo-entry))
    :use-module ((tegfs tegfs-login-by-key) :select (tegfs-login-by-key))
    :use-module ((tegfs webcore-context) :select (context-filemap/2))
    :use-module ((tegfs web-get-adam-info) :select (web::get-adam-info))
    )))



(define webcore::share-entry
  (lambda (web::context)
    (define filemap/2 (context-filemap/2 web::context))

    (profun-op-lambda
     :with-env env
     (ctx (E0 W) (E-name W-name))

     (define E (env (profun-meta-key E-name)))

     (define parent-senderid
       (and (profun-bound-value? E)
            (list? E)
            (assq-or keyword-entry-parent-directory-senderid E #f)))

     (define parent-info
       (and parent-senderid
            (filemap-ref-by-senderid filemap/2 parent-senderid #f)))

     (define adam-info
       (and parent-info
            (web::get-adam-info filemap/2 parent-info)))

     (define adam-entry
       (and adam-info (sharedinfo-entry adam-info)))

     (define id
       (or (and adam-entry
                (assq-or keyword-id adam-entry #f))
           (and (profun-bound-value? E)
                (list? E)
                (assq-or keyword-id E #f))))

     (define with-key
       (and (profun-bound-value? W)
            (string? W)
            W))

     (define with-permissions
       (and with-key
            (tegfs-login-by-key web::context with-key)))

     (define with-idset
       (and with-permissions
            (permission-idset with-permissions)))

     (cond
      ((not id)
       (make-profun-error
        'type-error
        "Entry does not have an id"
        "Maybe you don't have access to entry's id"))
      ((not with-key)
       (make-profun-error
        'type-error
        "Second argument expected to be a key (string) of the user that the entry is shared with, but it is not"))
      ((not with-permissions)
       (make-profun-error
        'user-not-found
        "Cannot find a user with that key"))
      ((and id with-idset)
       (hashset-add! with-idset id)
       (profun-accept))
      (else
       (raisu 'impossible-case))))))
