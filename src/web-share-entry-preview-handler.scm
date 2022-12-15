;;;; Copyright (C) 2022  Otto Jung
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

%var web-share-entry-preview-handler

%use (profun-set) "./euphrates/profun-accept.scm"
%use (make-profun-error) "./euphrates/profun-error.scm"
%use (profun-meta-key) "./euphrates/profun-meta-key.scm"
%use (profun-op-envlambda) "./euphrates/profun-op-envlambda.scm"
%use (profun-bound-value?) "./euphrates/profun-value.scm"
%use (default-preview-sharing-time) "./default-preview-sharing-time.scm"
%use (entry-target-fullpath) "./entry-target-fullpath.scm"
%use (get-preview-path) "./get-preview-path.scm"
%use (permission?) "./permission.scm"
%use (sharedinfo-senderid) "./sharedinfo.scm"
%use (query-permissions/p) "./talk-parameters.scm"
%use (web-share-file) "./web-share-file.scm"

(define web-share-entry-preview-handler
  (lambda (web-context)
    (profun-op-envlambda
     (ctx env (E-name R-name))

     (define perm (query-permissions/p))

     (define (continue target-fullpath)
       (define preview-fullpath (get-preview-path target-fullpath))
       (define info (web-share-file web-context perm preview-fullpath default-preview-sharing-time))
       (define vid (sharedinfo-senderid info))
       (profun-set (R-name <- vid)))

     (define (try entry)
       (define target-fullpath (entry-target-fullpath entry))
       (and target-fullpath
            (continue target-fullpath)))

     (cond
      ((permission? perm)
       (or (try (env E-name))
           (try (env (profun-meta-key E-name)))
           (make-profun-error 'bad-entry:does-not-have-target-infos)))
      ((profun-bound-value? perm)
       (make-profun-error 'type-error 'expecte-permissions perm))
      (else
       (make-profun-error 'missing-parameter 'permissions))))))
