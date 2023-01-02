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

%var web-senderid->entry-handler

%use (profun-set) "./euphrates/profun-accept.scm"
%use (make-profun-error) "./euphrates/profun-error.scm"
%use (profun-op-lambda) "./euphrates/profun-op-lambda.scm"
%use (profun-reject) "./euphrates/profun-reject.scm"
%use (profun-bound-value? profun-unbound-value?) "./euphrates/profun-value.scm"
%use (raisu) "./euphrates/raisu.scm"
%use (filemap-ref-by-senderid) "./filemap.scm"
%use (keyword-entry-parent-directory) "./keyword-entry-parent-directory.scm"
%use (keyword-entry-registry-path) "./keyword-entry-registry-path.scm"
%use (keyword-target) "./keyword-target.scm"
%use (keyword-title) "./keyword-title.scm"
%use (sharedinfo-entry) "./sharedinfo.scm"
%use (context-filemap/2) "./web-context.scm"

(define target-fields
  (list keyword-target
        keyword-title
        keyword-entry-parent-directory ;; FIXME: remove non-target fields
        keyword-entry-registry-path))

(define web-senderid->entry-handler
  (lambda (web-context)
    (define filemap/2 (context-filemap/2 web-context))

    (profun-op-lambda
     (ctx (senderid entry)
          (senderid-name entry-name))

     (define info (filemap-ref-by-senderid filemap/2 senderid #f))

     (cond
      ((profun-unbound-value? senderid)
       (make-profun-error 'type-error "Senderid must be given"))

      ((profun-bound-value? entry)
       (make-profun-error 'type-error "Entry variable be free"))

      ((not info)
       (profun-reject))

      (info
       (let ()
         (define entry0 (sharedinfo-entry info))
         (define entry
           (filter (lambda (p) (memq (car p) target-fields)) entry0))
         (profun-set (entry-name <- entry))))

      (else
       (raisu 'impossible-case))))))
