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

%var web-share-entry-generic-handler

%use (profun-set) "./euphrates/profun-accept.scm"
%use (make-profun-error) "./euphrates/profun-error.scm"
%use (profun-meta-key) "./euphrates/profun-meta-key.scm"
%use (profun-op-envlambda) "./euphrates/profun-op-envlambda.scm"
%use (profun-request-value) "./euphrates/profun-request-value.scm"
%use (profun-bound-value? profun-unbound-value?) "./euphrates/profun-value.scm"
%use (entry-target-fullpath) "./entry-target-fullpath.scm"
%use (permission?) "./permission.scm"
%use (sharedinfo-senderid) "./sharedinfo.scm"
%use (web-share-file/dont-link-yet) "./web-share-file.scm"
%use (webcore::permissions/p) "./webcore-parameters.scm"

(define web-share-entry-generic-handler
  (lambda (get-shared-path)
    (lambda (web-context)
      (profun-op-envlambda
       (ctx env (E-name T-name R-name))

       (define sharing-time (env T-name))
       (define perm (webcore::permissions/p))

       (define (continue entry target-fullpath)
         (define generic-fullpath (get-shared-path target-fullpath))
         (if generic-fullpath
             (let ()
               (define info
                 (web-share-file/dont-link-yet
                  web-context perm entry generic-fullpath sharing-time))
               (define vid (and info (sharedinfo-senderid info)))
               (if info
                   (profun-set (R-name <- vid))
                   (make-profun-error 'cannot-share-for-that-long)))
             (profun-set (R-name <- #f))))

       (define (try entry)
         (and (profun-bound-value? entry)
              (let ((target-fullpath (entry-target-fullpath entry)))
                (and target-fullpath
                     (continue entry target-fullpath)))))

       (cond
        ((profun-unbound-value? (env E-name))
         (profun-request-value E-name))
        ((profun-unbound-value? sharing-time)
         (profun-request-value T-name))
        ((not (and (number? sharing-time)
                   (< 0 sharing-time)))
         (make-profun-error
          'type-error "Sharing type must be a number greater than 0"
          sharing-time))
        ((permission? perm)
         (or (try (env (profun-meta-key E-name)))
             ;; (try (env E-name)) ;; NOTE: uncommenting allows forging of entries
             (make-profun-error 'bad-entry:does-not-have-target-infos (env E-name))))
        (else
         (make-profun-error 'could-not-authorize)))))))
