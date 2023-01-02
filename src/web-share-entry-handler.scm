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

%run guile

%var web-share-entry-handler

%use (assq-or) "./euphrates/assq-or.scm"
%use (hashset-add!) "./euphrates/hashset.scm"
%use (profun-accept) "./euphrates/profun-accept.scm"
%use (make-profun-error) "./euphrates/profun-error.scm"
%use (profun-op-lambda) "./euphrates/profun-op-lambda.scm"
%use (profun-bound-value?) "./euphrates/profun-value.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (filemap-ref-by-senderid) "./filemap.scm"
%use (keyword-entry-parent-directory-senderid) "./keyword-entry-parent-directory-senderid.scm"
%use (keyword-id) "./keyword-id.scm"
%use (permission-idset) "./permission.scm"
%use (sharedinfo-entry) "./sharedinfo.scm"
%use (tegfs-login-by-key) "./tegfs-login-by-key.scm"
%use (context-filemap/2) "./web-context.scm"
%use (web-get-adam-info) "./web-get-adam-info.scm"

(define web-share-entry-handler
  (lambda (web-context)
    (define filemap/2 (context-filemap/2 web-context))

    (profun-op-lambda
     (ctx (E W) (E-name W-name))

     (define parent-senderid
       (and (profun-bound-value? E)
            (list? E)
            (assq-or keyword-entry-parent-directory-senderid E #f)))

     (define parent-info
       (and parent-senderid
            (filemap-ref-by-senderid filemap/2 parent-senderid #f)))

     (define adam-info
       (and parent-info
            (web-get-adam-info filemap/2 parent-info)))

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
            (tegfs-login-by-key web-context with-key)))

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
